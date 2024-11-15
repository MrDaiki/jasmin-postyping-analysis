open Jasmin
open Prog
open State_walker
open Domain

module RdMutator : SimpleMutator with type state = Domain.t = struct
  type state = Domain.t

  let loop_stop_condition (prev : state) (state : state) : bool = Domain.included state prev

  let merge (_ : L.i_loc) (s1 : state) (s2 : state) : state = Domain.join s1 s2

  let merge_out_loop s1 s2 = Domain.join s1 s2

  let cond (_ : L.i_loc) (_ : int gexpr) (state : state) : state = state

  let cif (_ : L.i_loc) (_ : int gexpr) (state : state) : state * state = (state, state)

  let cassign
      (loc : L.i_loc)
      (lv : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (_ : int gexpr)
      (state : state) : state =
      Domain.add (written_lv Sv.empty lv) (Instruction loc) state

  let fcall (loc : L.i_loc) (lvs : int glvals) (_ : funname) (_ : int gexprs) (state : state) :
      state =
      Domain.add (written_lvs lvs) (Instruction loc) state

  let syscall
      (loc : L.i_loc)
      (lvs : int glvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (_ : int gexprs)
      (state : state) : state =
      Domain.add (written_lvs lvs) (Instruction loc) state

  let copn
      (loc : L.i_loc)
      (lvs : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (state : state) : state =
      Domain.add (written_lvs lvs) (Instruction loc) state
end

module RdWalker = SimpleWalker (RdMutator)
