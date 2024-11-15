open Jasmin
open Prog

module type SimpleMutator = sig
  type state

  val loop_stop_condition : state -> state -> bool

  val merge_out_loop : state -> state -> state

  val merge : L.i_loc -> state -> state -> state

  val cond : L.i_loc -> int gexpr -> state -> state

  val cif : L.i_loc -> int gexpr -> state -> state * state

  val cassign : L.i_loc -> int glval -> E.assgn_tag -> int gty -> int gexpr -> state -> state

  val fcall : L.i_loc -> int glvals -> funname -> int gexprs -> state -> state

  val syscall :
    L.i_loc -> int glvals -> BinNums.positive Syscall_t.syscall_t -> int gexprs -> state -> state

  val copn :
    L.i_loc -> int glval list -> E.assgn_tag -> 'asm Sopn.sopn -> int gexprs -> state -> state
end

module SimpleWalker (Mutator : SimpleMutator) = struct
  let build_assign_from_for (x : int gvar_i) direction gstart gend =
      let gend =
          if direction = E.UpTo then
            gend
          else
            gstart
      in
      let comp_op, assign_op =
          if direction = E.UpTo then
            (E.Olt Cmp_int, E.Oadd Op_int)
          else
            (E.Ogt Cmp_int, E.Osub Op_int)
      in
      let x_ggvar = {gv= x; gs= Slocal} in
      let cmp_expr = Papp2 (comp_op, Pvar x_ggvar, gend) in
      let incr_expr = Papp2 (assign_op, Pvar x_ggvar, Pconst (Z.of_int 1)) in
      (cmp_expr, incr_expr)

  let walk_assign lv tag gty expr loc prev =
      let state = Mutator.cassign loc lv tag gty expr prev in
      (Cassgn (lv, tag, gty, expr), state, prev)

  let rec walk_instr (state : Mutator.state) (instr : ('info, 'asm) instr) :
      Mutator.state * (int, Mutator.state, 'asm) ginstr =
      let out, news, annot = walk_instr_r instr.i_desc instr.i_loc state in
      (news, {i_desc= out; i_loc= instr.i_loc; i_info= annot; i_annot= instr.i_annot})

  and walk_while al body1 e body2 loc prev =
      let state, _ = walk_stmt body1 prev in
      let state = Mutator.cond loc e state in
      let rec loop prev =
          let out, body2 = walk_stmt body2 prev in
          let state, body1 = walk_stmt body1 out in
          let state = Mutator.cond loc e state in
          if Mutator.loop_stop_condition prev state then
            (Cwhile (al, body1, e, body2), state, out)
          else
            loop (Mutator.merge loc state prev)
      in
      let wh, os, out = loop state in
      (wh, Mutator.merge loc state os, Mutator.merge_out_loop out prev)

  and walk_for x (direction, gstart, gend) body loc prev =
      let _, state, _ =
          match direction with
          | E.UpTo -> walk_assign (Lvar x) AT_none (L.unloc x).v_ty gstart loc prev
          | E.DownTo -> walk_assign (Lvar x) AT_none (L.unloc x).v_ty gend loc prev
      in
      let cmp_exp, incr_exp = build_assign_from_for x direction gstart gend in
      let state = Mutator.cond loc cmp_exp state in
      let rec loop prev =
          let state, body = walk_stmt body prev in
          let _, state, _ = walk_assign (Lvar x) AT_none (L.unloc x).v_ty incr_exp loc state in
          let state = Mutator.cond loc cmp_exp state in
          if Mutator.loop_stop_condition prev state then
            (Cfor (x, (direction, gstart, gend), body), state)
          else
            loop (Mutator.merge loc state prev)
      in
      let forr, os = loop state in
      (forr, Mutator.merge loc state os, Mutator.merge_out_loop prev state)

  and walk_instr_r (instr : (int, 'info, 'asm) ginstr_r) (loc : L.i_loc) (prev : Mutator.state) :
      (int, Mutator.state, 'asm) ginstr_r * Mutator.state * Mutator.state =
      match instr with
      | Cassgn (lv, tag, gty, expr) -> walk_assign lv tag gty expr loc prev
      | Ccall (lvs, fname, args) ->
          let state = Mutator.fcall loc lvs fname args prev in
          (Ccall (lvs, fname, args), state, prev)
      | Csyscall (lvs, syscall, exprs) ->
          let state = Mutator.syscall loc lvs syscall exprs prev in
          (Csyscall (lvs, syscall, exprs), state, prev)
      | Copn (lvs, tag, sopn, exprs) ->
          let state = Mutator.copn loc lvs tag sopn exprs prev in
          (Copn (lvs, tag, sopn, exprs), state, prev)
      | Cif (e, th, el) ->
          let s1, s2 = Mutator.cif loc e prev in
          let s1, th = walk_stmt th s1 in
          let s2, el = walk_stmt el s2 in
          let state = Mutator.merge loc s1 s2 in
          (Cif (e, th, el), state, prev)
      | Cfor (x, gr, body) -> walk_for x gr body loc prev
      | Cwhile (al, body1, e, body2) -> walk_while al body1 e body2 loc prev

  and walk_stmt (stmt : (int, 'info, 'asm) gstmt) (state : Mutator.state) =
      List.fold_left_map walk_instr state stmt

  let walk_func (f : ('info, 'asm) func) (state : Mutator.state) =
      let state, body = walk_stmt f.f_body state in
      ( { f_loc= f.f_loc
        ; f_annot= f.f_annot
        ; f_cc= f.f_cc
        ; f_name= f.f_name
        ; f_tyin= f.f_tyin
        ; f_args= f.f_args
        ; f_body= body
        ; f_tyout= f.f_tyout
        ; f_outannot= f.f_outannot
        ; (* annotation attach to return type *)
          f_ret= f.f_ret }
      , state )
end

(*

A template of mutator for easy copy-paste : 

module Mutator : SimpleMutator with type state = unit = struct
  type state = unit

  let loop_stop_condition (prev : state) (state : state) : bool = true

  let merge_out_loop (s1 : state) (s2 : state) : state = s1

  let merge (loc : L.i_loc) (state : state) (state : state) : state = state

  let cond (loc : L.i_loc) (cond : int gexpr) (state : state) : state = state

  let cif (loc : L.i_loc) (cond : int gexpr) (state : state) : state * state = (state, state)

  let cassign
      (loc : L.i_loc)
      (lv : int glval)
      (tag : E.assgn_tag)
      (ty : int gty)
      (expr : int gexpr)
      (state : state) : state =
      state

  let fcall
      (loc : L.i_loc)
      (lvs : int glvals)
      (funname : funname)
      (exprs : int gexprs)
      (state : state) : state =
      state

  let syscall
      (loc : L.i_loc)
      (lvs : int glvals)
      (syscall : BinNums.positive Syscall_t.syscall_t)
      (exprs : int gexprs)
      (state : state) : state =
      state

  let copn
      (loc : L.i_loc)
      (lvs : int glvals)
      (tag : E.assgn_tag)
      (op : 'asm Sopn.sopn)
      (exprs : int gexprs)
      (state : state) : state =
      state
end

*)
