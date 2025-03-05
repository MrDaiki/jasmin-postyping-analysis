open Jasmin
open Prog
open Types

module type ForwardAnalyserLogic = sig
  (** type of annotation for the program*)
  type annotation

  (** Function that take input and output domain and convert them to annotation type*)

  (** Pretty printing function*)
  val pp_annot : Format.formatter -> Location.i_loc * annotation -> unit

  (** Incusion test 
  Check if an annotation is included in another one. 

  args :
  - prev : previous domain
  - domain : current domain 
  
  returns :
  - bool : true if the fixpoint is reached, false otherwise
  *)
  val included : annotation -> annotation -> bool

  (** 
  Function that return two annotations on a branching of the program.
  The first assume that the condition is true, the second that it is false
  args :
  - expr : condition of the branching
  - annotation : annotation before branching

  returns : 
  - annotation * annotation 

  *)
  val assume : expr -> annotation -> annotation * annotation

  (** Function that merge two annotations
  It is called at the end of branching in a program.
  args : 
  - annotation : first annotation
  - annotation : second annotation

  returns : 
  - annotation : merged annotation
  *)
  val merge : annotation -> annotation -> annotation

  (** Function that remove a proxy variable from the annotation if necessary 
    Some part of the tree walking will introduce proxy variables to handle some specific cases.
    This function is called to remove them from the annotation when they are not needed anymore
  *)
  val forget : var_i -> annotation -> annotation

  val funcall : Location.i_loc -> lvals -> funname -> exprs -> annotation -> annotation

  val syscall :
       Location.i_loc
    -> lvals
    -> BinNums.positive Syscall_t.syscall_t
    -> exprs
    -> annotation
    -> annotation

  val assign : Location.i_loc -> lval -> E.assgn_tag -> ty -> expr -> annotation -> annotation

  val opn :
    Location.i_loc -> lvals -> E.assgn_tag -> 'asm Sopn.sopn -> exprs -> annotation -> annotation
end

module ForwardAnalyser = struct
  module type S = sig
    (** anotation type*)
    type annotation

    (**
    Entrypoint for analysis. Using previous domain, the domain for the next instruction is computed. Annotation are then generated using `AnalyserLogic.to_annotation` function

    args : 
    - Prog.func (function to analyse)
    - annotation (initial annotation that will be modified during analysis)

    returns :
    - Prog.func (annotated function)
    *)
    val analyse_function : ('info, 'asm) Prog.func -> annotation -> (annotation, 'asm) Prog.func
  end

  (** Functor used to build TreeAnalyser modules*)
  module Make (Logic : ForwardAnalyserLogic) : S with type annotation = Logic.annotation = struct
    type annotation = Logic.annotation

    let analyse_assign (loc : Location.i_loc) (lv : lval) tag ty (expr : expr) domain =
        let domain = Logic.assign loc lv tag ty expr domain in
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
    let rec analyse_for (loc : Location.i_loc) variable (range : int grange) body in_domain :
        (int, annotation, 'asm) ginstr_r * annotation =
        (* Defining proxy variable *)
        let proxy_var = build_for_proxy_variable loc.base_loc variable in
        (* First assign to range begin*)
        let _, domain =
            analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
              (Grange.first range) in_domain
        in
        (* Building loop out condition *)
        let condition = build_for_condition_expr proxy_var range in
        let rec loop prev =
            let domain, result = Logic.assume condition prev in
            let _, domain =
                (* Assigning proxy_var to for variable*)
                analyse_assign loc (Lvar variable) AT_none (Location.unloc variable).v_ty
                  (Pvar {gv= proxy_var; gs= Slocal})
                  domain
            in
            let body, domain = analyse_stmt body domain in
            let _, domain =
                analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
                  (build_for_assign_expr proxy_var range)
                  domain
            in
            if Logic.included prev domain then
              (Cfor (variable, range, body), result)
            else
              loop (Logic.merge prev domain)
        in
        let body, domain = loop domain in
        (body, Logic.forget proxy_var domain (* Should we really remove the proxy_var ?*))

    and analyse_while
        (al : E.align)
        (cond : expr)
        ((a, _) : IInfo.t * 'info)
        (b1 : ('info, 'asm) stmt)
        (b2 : ('info, 'asm) stmt)
        (domain : annotation) =
        let rec loop (prev : annotation) =
            let b1, domain_s1 = analyse_stmt b1 prev in
            let domain, result = Logic.assume cond domain_s1 in
            let b2, domain_s2 = analyse_stmt b2 domain in
            if Logic.included prev domain_s2 then
              (Cwhile (al, b1, cond, (a, domain_s1), b2), result)
            else
              loop (Logic.merge domain_s2 prev)
        in
        let cwhile, result = loop domain in
        (cwhile, result)

    and analyse_instr_r
        (loc : Location.i_loc)
        (instr : (int, 'info, 'asm) ginstr_r)
        (domain : annotation) : (int, annotation, 'asm) ginstr_r * annotation =
        match instr with
        | Cassgn (lv, tag, ty, expr) -> analyse_assign loc lv tag ty expr domain
        | Copn (lvs, tag, sopn, es) ->
            let domain = Logic.opn loc lvs tag sopn es domain in
            (Copn (lvs, tag, sopn, es), domain)
        | Ccall (lvs, fn, es) ->
            let domain = Logic.funcall loc lvs fn es domain in
            (Ccall (lvs, fn, es), domain)
        | Csyscall (lvs, sc, es) ->
            let domain = Logic.syscall loc lvs sc es domain in
            (Csyscall (lvs, sc, es), domain)
        | Cif (expr, th, el) ->
            let domain_th, domain_el = Logic.assume expr domain in
            let th, domain_th = analyse_stmt th domain_th in
            let el, domain_el = analyse_stmt el domain_el in
            (Cif (expr, th, el), Logic.merge domain_th domain_el)
        | Cfor (var, range, bloc) -> analyse_for loc var range bloc domain
        | Cwhile (align, b1, cond, info, b2) -> analyse_while align cond info b1 b2 domain

    and analyse_instr (in_domain : annotation) (instr : ('info, 'asm) instr) :
        annotation * (annotation, 'asm) instr =
        let instr_r, out_domain = analyse_instr_r instr.i_loc instr.i_desc in_domain in
        (out_domain, {instr with i_desc= instr_r; i_info= in_domain})

    and analyse_stmt (stmt : ('info, 'asm) stmt) domain =
        let out_domain, stmt = List.fold_left_map analyse_instr domain stmt in
        (stmt, out_domain)

    let analyse_function (func : ('info, 'asm) Prog.func) (in_domain : annotation) :
        (annotation, 'asm) Prog.func =
        let body, out_domain = analyse_stmt func.f_body in_domain in
        {func with f_info= out_domain; f_body= body}
  end
end
