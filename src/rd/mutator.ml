open Jasmin
open Prog
open Walker
open Domain

module Rdmutator : Statemutator with type state = Domain.t and type annotation = Domain.t = struct
  type state = Domain.t

  type annotation = Domain.t

  let loop_stop_condition prev state = Domain.included state prev

  let cif _ _ out1 out2 =
      let state = Domain.join out1 out2 in
      (state, state)

  let cfor _ _ _ prev state =
      let state = Domain.join prev state in
      (state, state)

  let cfor_decl (lv : int gvar_i) _ loc state = Domain.add (Sv.singleton lv.pl_desc) loc state

  let cwhile _ _ _ prev state =
      let state = Domain.join prev state in
      (state, state)

  let cassign lv (_ : E.assgn_tag) (_ : int gty) (_ : int gexpr) (loc : L.i_loc) (state : state) =
      let state = Domain.add (written_lv Sv.empty lv) loc state in
      (state, state)

  let copn lvs _ _ _ loc state =
      let state = Domain.add (written_lvs lvs) loc state in
      (state, state)

  let fcall lvs (_ : funname) (_ : int gexprs) loc (state : state) =
      let state = Domain.add (written_lvs lvs) loc state in
      (state, state)

  let syscall lvs _ _ loc state =
      let state = Domain.add (written_lvs lvs) loc state in
      (state, state)
end

module RdWalker = TreeWalker (Rdmutator)
