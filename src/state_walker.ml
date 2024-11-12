open Jasmin
open Prog
open Wsize

module type SimpleMutator = sig
  type state

  val loop_stop_condition : state -> state -> bool

  val cif : int gexpr -> L.i_loc -> state -> state -> state

  val cloop : int gexpr -> L.i_loc -> state -> state -> state

  val cassign : int glval -> E.assgn_tag -> int gty -> int gexpr -> L.i_loc -> state -> state

  val fcall : int glvals -> funname -> int gexprs -> L.i_loc -> state -> state

  val syscall :
    int glvals -> BinNums.positive Syscall_t.syscall_t -> int gexprs -> L.i_loc -> state -> state

  val copn :
    int glval list -> E.assgn_tag -> 'asm Sopn.sopn -> int gexprs -> L.i_loc -> state -> state
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

  let walk_assign lv tag gty expr loc state =
      let state = Mutator.cassign lv tag gty expr loc state in
      (Cassgn (lv, tag, gty, expr), state)

  let rec walk_instr (state : Mutator.state) (instr : ('info, 'asm) instr) :
      Mutator.state * (int, Mutator.state, 'asm) ginstr =
      let out, state = walk_instr_r instr.i_desc instr.i_loc state in
      (state, {instr with i_desc= out; i_info= state})

  and walk_loop_body stmt1 stmt2 state =
      let state, stmt2 = walk_stmt stmt2 state in
      let state, stmt1 = walk_stmt stmt1 state in
      (stmt1, stmt2, state)

  and walk_while al body1 e body2 loc prev =
      let state, _ = walk_stmt body1 prev in
      let rec loop prev =
          let body1, body2, state = walk_loop_body body1 body2 prev in
          let state = Mutator.cloop e loc prev state in
          if Mutator.loop_stop_condition prev state then
            (Cwhile (al, body1, e, body2), state)
          else
            loop state
      in
      loop state

  and walk_for x (direction, gstart, gend) body loc prev =
      let _, state =
          match direction with
          | E.UpTo -> walk_assign (Lvar x) AT_none (L.unloc x).v_ty gstart loc prev
          | E.DownTo -> walk_assign (Lvar x) AT_none (L.unloc x).v_ty gend loc prev
      in
      let cmp_exp, incr_exp = build_assign_from_for x direction gstart gend in
      let rec loop prev =
          let _, body2, state = walk_loop_body [] body prev in
          let _, state = walk_assign (Lvar x) AT_none (L.unloc x).v_ty incr_exp loc state in
          let state = Mutator.cloop cmp_exp loc prev state in
          if Mutator.loop_stop_condition prev state then
            (Cfor (x, (direction, gstart, gend), body2), state)
          else
            loop state
      in
      loop state

  and walk_instr_r (instr : (int, 'info, 'asm) ginstr_r) (loc : L.i_loc) (state : Mutator.state) :
      (int, Mutator.state, 'asm) ginstr_r * Mutator.state =
      match instr with
      | Cassgn (lv, tag, gty, expr) -> walk_assign lv tag gty expr loc state
      | Ccall (lvs, fname, args) ->
          let state = Mutator.fcall lvs fname args loc state in
          (Ccall (lvs, fname, args), state)
      | Csyscall (lvs, syscall, exprs) ->
          let state = Mutator.syscall lvs syscall exprs loc state in
          (Csyscall (lvs, syscall, exprs), state)
      | Copn (lvs, tag, sopn, exprs) ->
          let state = Mutator.copn lvs tag sopn exprs loc state in
          (Copn (lvs, tag, sopn, exprs), state)
      | Cif (e, th, el) ->
          let s1, th = walk_stmt th state in
          let s2, el = walk_stmt el state in
          let state = Mutator.cif e loc s1 s2 in
          (Cif (e, th, el), state)
      | Cfor (x, gr, body) -> walk_for x gr body loc state
      | Cwhile (al, body1, e, body2) -> walk_while al body1 e body2 loc state

  and walk_stmt (stmt : (int, 'info, 'asm) gstmt) (state : Mutator.state) =
      List.fold_left_map walk_instr state stmt

  let walk_func (f : ('info, 'asm) func) (state : Mutator.state) =
      let state, body = walk_stmt f.f_body state in
      ({f with f_body= body}, state)
end

(*
   module ExampleMutator : SimpleMutator = struct
       type state = unit

       type constant = unit
       (* function name -> mutable pointer variables and their position in arg list *)

       let loop_stop_condition _ _ _ = true

       let cif (const : constant) (expr : int gexpr) (loc : L.i_loc) (th_state : state) (el_state : state)
           : state =
           th_state

       let cfor
           (const : constant)
           (loop_var : int gvar_i)
           (range : int grange)
           (loc : L.i_loc)
           (prev : state)
           (after : state) : state =
           after

       let cfor_decl
           (const : constant)
           (loop_var : int gvar_i)
           (range : int grange)
           (loc : L.i_loc)
           (state : state) : state =
           state

       let cwhile
           (const : constant)
           (align : E.align)
           (condition : int gexpr)
           (loc : L.i_loc)
           (b1_state : state)
           (b2_state : state) : state =
           b2_state

       let cassign
           (const : constant)
           (lv : int glval)
           (tag : E.assgn_tag)
           (ty : int gty)
           (expr : int gexpr)
           (loc : L.i_loc)
           (state : state) : state =
           state

       let fcall
           (const : constant)
           (lv : int glvals)
           (fname : funname)
           (args : int gexprs)
           (loc : L.i_loc)
           (state : state) : state =
           state

       let syscall
           (const : constant)
           (lv : int glvals)
           (syscall : BinNums.positive Syscall_t.syscall_t)
           (args : int gexprs)
           (loc : L.i_loc)
           (state : state) : state =
           state

       let copn
           (const : constant)
           (lvs : int glval list)
           (tag : E.assgn_tag)
           (opn : 'asm Sopn.sopn)
           (args : int gexprs)
           (loc : L.i_loc)
           (state : state) : state =
           state
     end
*)
