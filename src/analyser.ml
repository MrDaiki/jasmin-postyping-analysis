open Jasmin
open Prog
open Types.Grange

module type AnalyserLogic = sig
  (** type of annotation for the program*)
  type annotation

  type domain

  (** Incusion test 
  Check if an annotation is included in another one. 

  args :
  - prev : previous annotation
  - state : current annotation 
  
  returns :
  - bool : true if the fixpoint is reached, false otherwise
  *)

  val to_annotation : domain -> domain -> annotation

  val pp_annot : Format.formatter -> Location.i_loc * annotation -> unit

  val included : domain -> domain -> bool

  (** 
  Function that return two annotations on a branching of the program.
  The first assume that the condition is true, the second that it is false
  args :
  - int gexpr : condition of the branching
  - annotation : annotation before branching

  returns : 
  - annotation * annotation 

  *)
  val assume : int gexpr -> domain -> domain * domain

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
  val forget : int gvar_i -> domain -> domain

  val funcall : Location.i_loc -> int glvals -> funname -> int gexprs -> domain -> domain

  val syscall :
       Location.i_loc
    -> int glvals
    -> BinNums.positive Syscall_t.syscall_t
    -> int gexprs
    -> domain
    -> domain

  val assign :
    Location.i_loc -> int glval -> E.assgn_tag -> int gty -> int gexpr -> domain -> domain

  val opn :
       Location.i_loc
    -> int glval list
    -> E.assgn_tag
    -> 'asm Sopn.sopn
    -> int gexprs
    -> domain
    -> domain
end

module ForwardAnalyser = struct
  module type S = sig
    (** anotation type*)
    type annotation

    type domain

    (**
    Entrypoint for analysis. Each instruction is annotated with the entry annotation. Annotation for the next instruction is then computed

    args : 
    - Prog.func (function to analyse)
    - annotation (initial annotation that will be modified during analysis)

    returns :
    - Prog.func (annotated function)
    - annotation (out annotation)
    *)
    val analyse_function :
      ('info, 'asm) Prog.func -> domain -> (annotation, 'asm) Prog.func * domain
  end

  (** Functor used to build TreeAnalyser modules*)
  module Make (L : AnalyserLogic) :
    S with type annotation = L.annotation and type domain = L.domain = struct
    type annotation = L.annotation

    type domain = L.domain

    let analyse_assign (loc : Location.i_loc) (lv : int glval) tag ty (expr : int gexpr) state =
        let state = L.assign loc lv tag ty expr state in
        (Cassgn (lv, tag, ty, expr), state)

    let build_for_proxy_variable loc (x : int gvar_i) : var_i =
        Location.mk_loc loc (GV.clone (Location.unloc x))

    let build_for_assign_expr (x : int gvar_i) (r : int grange) : int gexpr =
        let assign_op = GR.incr_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (assign_op, Pvar x_ggvar, Pconst (Z.of_int 1))

    let build_for_condition_expr (x : int gvar_i) (r : int grange) : int gexpr =
        let gend = GR.last r in
        let comp_op = GR.cmp_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (comp_op, Pvar x_ggvar, gend)

    let rec analyse_for (loc : Location.i_loc) variable (range : int grange) body in_state :
        (int, annotation, 'asm) ginstr_r * domain =
        let proxy_var = build_for_proxy_variable loc.base_loc variable in
        let _, state =
            analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
              (GR.first range) in_state
        in
        let condition = build_for_condition_expr proxy_var range in
        let rec loop prev =
            let state, result = L.assume condition prev in
            let _, state =
                analyse_assign loc (Lvar variable) AT_none (Location.unloc variable).v_ty
                  (Pvar {gv= proxy_var; gs= Slocal})
                  state
            in
            let body, state = analyse_stmt body state in
            let _, state =
                analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
                  (build_for_assign_expr proxy_var range)
                  state
            in
            if L.included prev state then
              (Cfor (variable, range, body), result)
            else
              loop (L.merge prev state)
        in
        let body, state = loop state in
        (body, L.forget proxy_var state)

    and analyse_while
        (al : E.align)
        (cond : int gexpr)
        ((a, _) : IInfo.t * 'info)
        (b1 : (int, 'info, 'asm) gstmt)
        (b2 : (int, 'info, 'asm) gstmt)
        (state : domain) =
        let rec loop (prev : domain) =
            let b1, state_s1 = analyse_stmt b1 prev in
            let state, result = L.assume cond state_s1 in
            let b2, state_s2 = analyse_stmt b2 state in
            if L.included prev state_s2 then
              (Cwhile (al, b1, cond, (a, L.to_annotation state_s1 result), b2), result)
            else
              loop (L.merge state_s2 prev)
        in
        let cwhile, result = loop state in
        (cwhile, result)

    and analyse_instr_r (loc : Location.i_loc) (instr : (int, 'info, 'asm) ginstr_r) (state : domain)
        : (int, annotation, 'asm) ginstr_r * domain =
        match instr with
        | Cassgn (lv, tag, ty, expr) -> analyse_assign loc lv tag ty expr state
        | Copn (lvs, tag, sopn, es) ->
            let annot = L.opn loc lvs tag sopn es state in
            (Copn (lvs, tag, sopn, es), annot)
        | Ccall (lvs, fn, es) ->
            let annot = L.funcall loc lvs fn es state in
            (Ccall (lvs, fn, es), annot)
        | Csyscall (lvs, sc, es) ->
            let annot = L.syscall loc lvs sc es state in
            (Csyscall (lvs, sc, es), annot)
        | Cif (expr, th, el) ->
            let annot_th, annot_el = L.assume expr state in
            let th, annot_th = analyse_stmt th annot_th in
            let el, annot_el = analyse_stmt el annot_el in
            (Cif (expr, th, el), L.merge annot_th annot_el)
        | Cfor (var, range, bloc) -> analyse_for loc var range bloc state
        | Cwhile (align, b1, cond, info, b2) -> analyse_while align cond info b1 b2 state

    and analyse_instr (in_domain : domain) (instr : ('info, 'asm) instr) :
        domain * (annotation, 'asm) instr =
        let instr_r, out_domain = analyse_instr_r instr.i_loc instr.i_desc in_domain in
        ( out_domain
        , { i_desc= instr_r
          ; i_loc= instr.i_loc
          ; i_info= L.to_annotation in_domain out_domain
          ; i_annot= instr.i_annot } )

    and analyse_stmt (stmt : ('info, 'asm) stmt) domain =
        let out_domain, stmt = List.fold_left_map analyse_instr domain stmt in
        (stmt, out_domain)

    let analyse_function (func : ('info, 'asm) Prog.func) (in_domain : domain) :
        (annotation, 'asm) Prog.func * domain =
        let body, out_domain = analyse_stmt func.f_body in_domain in
        ( { f_loc= func.f_loc
          ; f_annot= func.f_annot
          ; f_cc= func.f_cc
          ; f_info= L.to_annotation in_domain out_domain
          ; f_name= func.f_name
          ; f_tyin= func.f_tyin
          ; f_args= func.f_args
          ; f_body= body
          ; f_tyout= func.f_tyout
          ; f_outannot= func.f_outannot
          ; f_ret= func.f_ret }
        , out_domain )
  end
end
