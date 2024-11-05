open Jasmin
open Utils
open Prog

module type Statemutator = sig
  type state

  type annotation

  val loop_stop_condition : state -> state -> bool

  val cif : int gexpr -> L.i_loc -> state -> state -> state * annotation

  val cfor : int gvar_i -> int grange -> L.i_loc -> state -> state -> state * annotation

  val cfor_decl : int gvar_i -> int grange -> L.i_loc -> state -> state

  val cwhile : E.align -> int gexpr -> L.i_loc -> state -> state -> state * annotation

  val cassign :
    int glval -> E.assgn_tag -> int gty -> int gexpr -> L.i_loc -> state -> state * annotation

  val fcall : int glvals -> funname -> int gexprs -> L.i_loc -> state -> state * annotation

  val syscall :
       int glvals
    -> BinNums.positive Syscall_t.syscall_t
    -> int gexprs
    -> L.i_loc
    -> state
    -> state * annotation

  val copn :
       int glval list
    -> E.assgn_tag
    -> 'asm Sopn.sopn
    -> int gexprs
    -> L.i_loc
    -> state
    -> state * annotation
end

(*
When do we give control to the mutator ? 
*)
module TreeWalker (Mutator : Statemutator) = struct
  let rec walk_instr (state : Mutator.state) (instr : ('info, 'asm) instr) :
      Mutator.state * (int, Mutator.annotation, 'asm) ginstr =
      let out, state, annot = walk_instr_r instr.i_desc instr.i_loc state in
      (state, {instr with i_desc= out; i_info= annot})

  and walk_while al body1 e body2 loc prev =
      let state, _ = walk_stmt body1 prev in
      let rec loop prev =
          let state, body2 = walk_stmt body2 prev in
          let state, body1 = walk_stmt body1 state in
          let state, annot = Mutator.cwhile al e loc prev state in
          if Mutator.loop_stop_condition prev state then
            (Cwhile (al, body1, e, body2), state, annot)
          else
            loop state
      in
      loop state

  and walk_for x gr body loc prev =
      let rec loop prev =
          let state = Mutator.cfor_decl x gr loc prev in
          let state, body = walk_stmt body state in
          let state, annot = Mutator.cfor x gr loc prev state in
          if Mutator.loop_stop_condition prev state then
            (Cfor (x, gr, body), state, annot)
          else
            loop state
      in
      loop prev

  and walk_instr_r (instr : (int, 'info, 'asm) ginstr_r) (loc : L.i_loc) (state : Mutator.state) :
      (int, Mutator.annotation, 'asm) ginstr_r * Mutator.state * Mutator.annotation =
      match instr with
      | Cassgn (lv, tag, gty, expr) ->
          let state, annot = Mutator.cassign lv tag gty expr loc state in
          (Cassgn (lv, tag, gty, expr), state, annot)
      | Ccall (lvs, fname, args) ->
          let state, annot = Mutator.fcall lvs fname args loc state in
          (Ccall (lvs, fname, args), state, annot)
      | Csyscall (lvs, syscall, exprs) ->
          let state, annot = Mutator.syscall lvs syscall exprs loc state in
          (Csyscall (lvs, syscall, exprs), state, annot)
      | Copn (lvs, tag, sopn, exprs) ->
          let state, annot = Mutator.copn lvs tag sopn exprs loc state in
          (Copn (lvs, tag, sopn, exprs), state, annot)
      | Cif (e, th, el) ->
          let s1, th = walk_stmt th state in
          let s2, el = walk_stmt el state in
          let state, annot = Mutator.cif e loc s1 s2 in
          (Cif (e, th, el), state, annot)
      | Cfor (x, gr, body) -> walk_for x gr body loc state
      | Cwhile (al, body1, e, body2) -> walk_while al body1 e body2 loc state

  and walk_stmt (stmt : (int, 'info, 'asm) gstmt) (state : Mutator.state) =
      List.fold_left_map walk_instr state stmt

  let walk_func (f : ('info, 'asm) func) (state : Mutator.state) =
      let state, body = walk_stmt f.f_body state in
      ({f with f_body= body}, state)
end
