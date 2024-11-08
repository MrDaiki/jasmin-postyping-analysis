open Jasmin
open Prog

module type SimpleMutator = sig
  type state

  type constant

  val loop_stop_condition : constant -> state -> state -> bool

  val cif : constant -> int gexpr -> L.i_loc -> state -> state -> state

  val cfor : constant -> int gvar_i -> int grange -> L.i_loc -> state -> state -> state

  val cfor_decl : constant -> int gvar_i -> int grange -> L.i_loc -> state -> state

  val cwhile : constant -> E.align -> int gexpr -> L.i_loc -> state -> state -> state

  val cassign :
    constant -> int glval -> E.assgn_tag -> int gty -> int gexpr -> L.i_loc -> state -> state

  val fcall : constant -> int glvals -> funname -> int gexprs -> L.i_loc -> state -> state

  val syscall :
       constant
    -> int glvals
    -> BinNums.positive Syscall_t.syscall_t
    -> int gexprs
    -> L.i_loc
    -> state
    -> state

  val copn :
       constant
    -> int glval list
    -> E.assgn_tag
    -> 'asm Sopn.sopn
    -> int gexprs
    -> L.i_loc
    -> state
    -> state
end

module StateWalker (Mutator : SimpleMutator) = struct
  let rec walk_instr
      (constant : Mutator.constant)
      (state : Mutator.state)
      (instr : ('info, 'asm) instr) : Mutator.state * (int, Mutator.state, 'asm) ginstr =
      let out, state = walk_instr_r constant instr.i_desc instr.i_loc state in
      (state, {instr with i_desc= out; i_info= state})

  and walk_while constant al body1 e body2 loc prev =
      let state, _ = walk_stmt constant body1 prev in
      let rec loop prev =
          let state, body2 = walk_stmt constant body2 prev in
          let state, body1 = walk_stmt constant body1 state in
          let state = Mutator.cwhile constant al e loc prev state in
          if Mutator.loop_stop_condition constant prev state then
            (Cwhile (al, body1, e, body2), state)
          else
            loop state
      in
      loop state

  and walk_for constant x gr body loc prev =
      let rec loop prev =
          let state = Mutator.cfor_decl constant x gr loc prev in
          let state, body = walk_stmt constant body state in
          let state = Mutator.cfor constant x gr loc prev state in
          if Mutator.loop_stop_condition constant prev state then
            (Cfor (x, gr, body), state)
          else
            loop state
      in
      loop prev

  and walk_instr_r
      (constant : Mutator.constant)
      (instr : (int, 'info, 'asm) ginstr_r)
      (loc : L.i_loc)
      (state : Mutator.state) : (int, Mutator.state, 'asm) ginstr_r * Mutator.state =
      match instr with
      | Cassgn (lv, tag, gty, expr) ->
          let state = Mutator.cassign constant lv tag gty expr loc state in
          (Cassgn (lv, tag, gty, expr), state)
      | Ccall (lvs, fname, args) ->
          let state = Mutator.fcall constant lvs fname args loc state in
          (Ccall (lvs, fname, args), state)
      | Csyscall (lvs, syscall, exprs) ->
          let state = Mutator.syscall constant lvs syscall exprs loc state in
          (Csyscall (lvs, syscall, exprs), state)
      | Copn (lvs, tag, sopn, exprs) ->
          let state = Mutator.copn constant lvs tag sopn exprs loc state in
          (Copn (lvs, tag, sopn, exprs), state)
      | Cif (e, th, el) ->
          let s1, th = walk_stmt constant th state in
          let s2, el = walk_stmt constant el state in
          let state = Mutator.cif constant e loc s1 s2 in
          (Cif (e, th, el), state)
      | Cfor (x, gr, body) -> walk_for constant x gr body loc state
      | Cwhile (al, body1, e, body2) -> walk_while constant al body1 e body2 loc state

  and walk_stmt
      (constant : Mutator.constant)
      (stmt : (int, 'info, 'asm) gstmt)
      (state : Mutator.state) =
      List.fold_left_map (walk_instr constant) state stmt

  let walk_func (constant : Mutator.constant) (f : ('info, 'asm) func) (state : Mutator.state) =
      let state, body = walk_stmt constant f.f_body state in
      ({f with f_body= body}, state)
end
