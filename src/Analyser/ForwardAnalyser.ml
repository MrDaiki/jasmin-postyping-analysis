open Jasmin
open Prog
open Types
open Annotation

module type ForwardAnalyserLogic = sig
  (** type of annotation for the program*)
  type domain

  (** Function that take input and output domain and convert them to annotation type*)

  (** Pretty printing function*)
  val pp_annot : Format.formatter -> Location.i_loc * domain -> unit

  (** Incusion test 
  Check if an annotation is included in another one. 

  args :
  - prev : previous domain
  - domain : current domain 
  
  returns :
  - bool : true if the fixpoint is reached, false otherwise
  *)
  val included : domain -> domain -> bool

  (** 
  Function that return two annotations on a branching of the program.
  The first assume that the condition is true, the second that it is false
  args :
  - expr : condition of the branching
  - annotation : annotation before branching

  returns : 
  - annotation * annotation 

  *)
  val assume : expr -> domain -> domain annotation * domain annotation

  (** Function that merge two annotations
  It is called at the end of branching in a program.
  args : 
  - annotation : first annotation
  - annotation : second annotation

  returns : 
  - annotation : merged annotation
  *)
  val merge : domain -> domain -> domain

  (** Function that remove a proxy variable from the annotation if necessary 
    Some part of the tree walking will introduce proxy variables to handle some specific cases.
    This function is called to remove them from the annotation when they are not needed anymore
  *)
  val forget : domain -> var_i -> domain annotation

  val funcall : Location.i_loc -> lvals -> funname -> exprs -> domain -> domain annotation

  val syscall :
       Location.i_loc
    -> lvals
    -> BinNums.positive Syscall_t.syscall_t
    -> exprs
    -> domain
    -> domain annotation

  val assign : Location.i_loc -> lval -> E.assgn_tag -> ty -> expr -> domain -> domain annotation

  val opn :
    Location.i_loc -> lvals -> E.assgn_tag -> 'asm Sopn.sopn -> exprs -> domain -> domain annotation

  val initial_domain : ('info, 'asm) func -> domain annotation
end

module ForwardAnalyser = struct
  module type S = sig
    type domain

    (**
    Entrypoint for analysis. Using previous domain, the domain for the next instruction is computed. Annotation are then generated using `AnalyserLogic.to_annotation` function

    args : 
    - Prog.func (function to analyse)
    - annotation (initial annotation that will be modified during analysis)

    returns :
    - Prog.func (annotated function)
    *)
    val analyse_function : ('info, 'asm) Prog.func -> (domain annotation, 'asm) Prog.func
  end

  (** Functor used to build TreeAnalyser modules*)
  module Make (Logic : ForwardAnalyserLogic) : S with type domain = Logic.domain = struct
    type domain = Logic.domain

    type annot = Logic.domain annotation

    let merge (a1 : annot) (a2 : annot) =
        match (a1, a2) with
        | Empty, _ -> a2
        | _, Empty -> a1
        | Annotation d1, Annotation d2 -> Annotation (Logic.merge d1 d2)

    let assume (annotation : annot) (expr : expr) : annot * annot =
        match annotation with
        | Empty -> (Empty, Empty)
        | Annotation d -> Logic.assume expr d

    let forget (annotation : annot) (var : var_i) : annot =
        match annotation with
        | Empty -> Empty
        | Annotation d -> Logic.forget d var

    let included (prev : annot) (domain : annot) : bool =
        match (prev, domain) with
        | Empty, _ -> true
        | _, Empty -> false
        | Annotation d1, Annotation d2 -> Logic.included d1 d2

    let analyse_assign (loc : Location.i_loc) (lv : lval) tag ty (expr : expr) (annotation : annot)
        =
        let domain = Logic.assign loc lv tag ty expr annotation in
        (Cassgn (lv, tag, ty, expr), domain)

    let build_for_proxy_variable loc (x : var_i) : var_i =
        Location.mk_loc loc (GV.clone (Location.unloc x))

    let build_for_assign_expr (x : var_i) (r : int grange) : expr =
        let assign_op = Grange.incr_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (assign_op, Pvar x_ggvar, Pconst (Z.of_int 1))

    let build_for_condition_expr (x : var_i) (r : int grange) : expr =
        let gend = Grange.last r in
        let comp_op = Grange.cmp_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (comp_op, Pvar x_ggvar, gend)

    (**
    For function analysis. To analyse 
    *)
    let rec analyse_for
        (loc : Location.i_loc)
        variable
        (range : int grange)
        body
        (in_annomtation : annot) : (int, annot, 'asm) ginstr_r * annot =
        (* Defining proxy variable *)
        let proxy_var = build_for_proxy_variable loc.base_loc variable in
        (* First assign to range begin*)
        let _, annotation =
            analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
              (Grange.first range) in_annomtation
        in
        (* Building loop out condition *)
        let condition = build_for_condition_expr proxy_var range in
        let rec loop (prev : annot) =
            let annotation, out_annotation = assume prev condition in
            let _, annotation =
                (* Assigning proxy_var to for variable*)
                analyse_assign loc (Lvar variable) AT_none (Location.unloc variable).v_ty
                  (Pvar {gv= proxy_var; gs= Slocal})
                  annotation
            in
            let body, annotation = analyse_stmt body annotation in
            let _, domain =
                analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
                  (build_for_assign_expr proxy_var range)
                  annotation
            in
            if included prev domain then
              (Cfor (variable, range, body), out_annotation)
            else
              loop (merge prev domain)
        in
        let body, annotation = loop annotation in
        (body, forget annotation proxy_var)

    and analyse_while
        (al : E.align)
        (cond : expr)
        ((a, _) : IInfo.t * 'info)
        (b1 : ('info, 'asm) stmt)
        (b2 : ('info, 'asm) stmt)
        (annotation : annot) =
        let rec loop (prev : annot) =
            let b1, annotation_s1 = analyse_stmt b1 prev in
            let annotation, result = assume annotation_s1 cond in
            let b2, annotation_s2 = analyse_stmt b2 annotation in
            if included prev annotation_s2 then
              (Cwhile (al, b1, cond, (a, annotation_s1), b2), result)
            else
              loop (merge annotation_s2 prev)
        in
        let cwhile, result = loop annotation in
        (cwhile, result)

    and analyse_instr_r
        (loc : Location.i_loc)
        (instr : (int, 'info, 'asm) ginstr_r)
        (annotation : annot) : (int, annot, 'asm) ginstr_r * annot =
        match instr with
        | Cassgn (lv, tag, ty, expr) -> analyse_assign loc lv tag ty expr annotation
        | Copn (lvs, tag, sopn, es) ->
            let annotation = Logic.opn loc lvs tag sopn es annotation in
            (Copn (lvs, tag, sopn, es), annotation)
        | Ccall (lvs, fn, es) ->
            let annotation = Logic.funcall loc lvs fn es annotation in
            (Ccall (lvs, fn, es), annotation)
        | Csyscall (lvs, sc, es) ->
            let annotation = Logic.syscall loc lvs sc es annotation in
            (Csyscall (lvs, sc, es), annotation)
        | Cif (expr, th, el) ->
            let domain_th, domain_el = assume annotation expr in
            let th, domain_th = analyse_stmt th domain_th in
            let el, domain_el = analyse_stmt el domain_el in
            (Cif (expr, th, el), merge domain_th domain_el)
        | Cfor (var, range, bloc) -> analyse_for loc var range bloc annotation
        | Cwhile (align, b1, cond, info, b2) -> analyse_while align cond info b1 b2 annotation

    and analyse_instr (in_annotation : annot) (instr : ('info, 'asm) instr) :
        annot * (annot, 'asm) instr =
        let instr_r, out_annotation = analyse_instr_r instr.i_loc instr.i_desc in_annotation in
        (out_annotation, {instr with i_desc= instr_r; i_info= in_annotation})

    and analyse_stmt (stmt : ('info, 'asm) stmt) in_annotation =
        let out_annotation, stmt = List.fold_left_map analyse_instr in_annotation stmt in
        (stmt, out_annotation)

    let analyse_function (func : ('info, 'asm) Prog.func) : (annot, 'asm) Prog.func =
        let in_annotation = Logic.initial_domain func in
        let body, out_annotation = analyse_stmt func.f_body in_annotation in
        {func with f_info= out_annotation; f_body= body}
  end
end
