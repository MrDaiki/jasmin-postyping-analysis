open Jasmin
open Prog

module type AnalyserLogic = sig
  type annotation

  val initial_annotation : annotation

  val fixpoint_condition : annotation -> annotation -> bool

  val condition_split : int gexpr -> annotation -> annotation * annotation

  val merge : annotation -> annotation -> annotation

  val funcall : Location.i_loc -> int glvals -> funname -> int gexprs -> annotation -> annotation

  val syscall :
       Location.i_loc
    -> int glvals
    -> BinNums.positive Syscall_t.syscall_t
    -> int gexprs
    -> annotation
    -> annotation

  val assign :
    Location.i_loc -> int glval -> E.assgn_tag -> int gty -> int gexpr -> annotation -> annotation

  val opn :
       Location.i_loc
    -> int glval list
    -> E.assgn_tag
    -> 'asm Sopn.sopn
    -> int gexprs
    -> annotation
    -> annotation
end

module Analyser = struct
  module type S = sig
    type annotation

    val analyse_function : ('info, 'asm) Prog.func -> (annotation, 'asm) Prog.func * annotation
  end

  module Make (L : AnalyserLogic) : S with type annotation = L.annotation = struct
    type annotation = L.annotation

    let analyse_assign (lv : int glval) tag ty (expr : int gexpr) state =
        (Cassgn (lv, tag, ty, expr), state)

    let build_for_assign_expr (x : int gvar_i) ((direction, _, _) : int grange) : int gexpr =
        let assign_op =
            if direction = E.UpTo then
              E.Oadd Op_int
            else
              E.Osub Op_int
        in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (assign_op, Pvar x_ggvar, Pconst (Z.of_int 1))

    let build_for_condition_expr (x : int gvar_i) ((direction, gstart, gend) : int grange) :
        int gexpr =
        let gend =
            if direction = E.UpTo then
              gend
            else
              gstart
        in
        let comp_op =
            if direction = E.UpTo then
              E.Olt Cmp_int
            else
              E.Ogt Cmp_int
        in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (comp_op, Pvar x_ggvar, gend)

    let rec analyse_for variable (range : int grange) body in_state :
        (int, annotation, 'asm) ginstr_r * annotation =
        let analyse_assign =
            analyse_assign (Lvar variable) AT_none (Location.unloc variable).v_ty
              (build_for_assign_expr variable range)
        in
        let condition_split = L.condition_split (build_for_condition_expr variable range) in
        let _, state = analyse_assign in_state in
        let rec loop prev =
            let body, state = analyse_stmt body state in
            let state, _ = condition_split state in
            let _, state = analyse_assign state in
            if L.fixpoint_condition prev state then
              (Cfor (variable, range, body), state)
            else
              loop (L.merge prev state)
        in
        loop state

    and analyse_while
        (al : E.align)
        (cond : int gexpr)
        ((a, _) : IInfo.t * 'info)
        (b1 : (int, 'info, 'asm) gstmt)
        (b2 : (int, 'info, 'asm) gstmt)
        (state : annotation) =
        let _, state = analyse_stmt b1 state in
        let state, _ = L.condition_split cond state in
        let rec loop (prev : annotation) =
            let b2, state_s2 = analyse_stmt b2 prev in
            let b1, state_s1 = analyse_stmt b1 state_s2 in
            let state, _ = L.condition_split cond state_s1 in
            if L.fixpoint_condition prev state then
              (Cwhile (al, b1, cond, (a, state_s1), b2), state_s2)
            else
              loop (L.merge state prev)
        in
        loop state

    and analyse_instr_r
        (loc : Location.i_loc)
        (instr : (int, 'info, 'asm) ginstr_r)
        (state : annotation) : (int, annotation, 'asm) ginstr_r * annotation =
        match instr with
        | Cassgn (lv, tag, ty, expr) ->
            let annot = L.assign loc lv tag ty expr state in
            (Cassgn (lv, tag, ty, expr), annot)
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
            let annot_th, annot_el = L.condition_split expr state in
            let th, annot_th = analyse_stmt th annot_th in
            let el, annot_el = analyse_stmt el annot_el in
            (Cif (expr, th, el), L.merge annot_th annot_el)
        | Cfor (var, range, bloc) -> analyse_for var range bloc state
        | Cwhile (align, b1, cond, info, b2) -> analyse_while align cond info b1 b2 state

    and analyse_instr state (instr : ('info, 'asm) instr) : annotation * (annotation, 'asm) instr =
        let instr_r, annot = analyse_instr_r instr.i_loc instr.i_desc state in
        (annot, {i_desc= instr_r; i_loc= instr.i_loc; i_info= annot; i_annot= instr.i_annot})

    and analyse_stmt (stmt : ('info, 'asm) stmt) annotation =
        let a, b = List.fold_left_map analyse_instr annotation stmt in
        (b, a)

    let analyse_function (func : ('info, 'asm) Prog.func) :
        (annotation, 'asm) Prog.func * annotation =
        let initial_annot = L.initial_annotation in
        let body, annot = analyse_stmt func.f_body initial_annot in
        ( { f_loc= func.f_loc
          ; f_annot= func.f_annot
          ; f_cc= func.f_cc
          ; f_name= func.f_name
          ; f_tyin= func.f_tyin
          ; f_args= func.f_args
          ; f_body= body
          ; f_tyout= func.f_tyout
          ; f_outannot= func.f_outannot
          ; f_ret= func.f_ret }
        , annot )
  end
end
