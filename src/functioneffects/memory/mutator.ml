open Jasmin
open Memoryeffect
open State_walker
open Utils
open Prog

let memory_effect_lv (lv : int glval) : memory_effect =
    match lv with
    | Lmem _ -> Some
    | _ -> None

let memory_effect_lvs (lvs : int glvals) : memory_effect =
    List.fold_left (fun acc lv -> acc || memory_effect_lv lv) None lvs

module MemoryEffectMutator : SimpleMutator with type state = memory_effect = struct
  type state = memory_effect

  let loop_stop_condition (_ : state) (_ : state) : bool = true

  let merge (_ : L.i_loc) (s1 : state) (s2 : state) : state = s1 || s2

  let cond (_ : L.i_loc) (_ : int gexpr) (state : state) : state = state

  let cif (_ : L.i_loc) (_ : int gexpr) (state : state) : state * state = (state, state)

  let cassign
      (_ : L.i_loc)
      (lv : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (_ : int gexpr)
      (state : state) : state =
      state || memory_effect_lv lv

  let fcall (_ : L.i_loc) (lvs : int glvals) (funname : funname) (_ : int gexprs) (state : state) :
      state =
      state || memory_effect_lvs lvs || Depends (Sf.singleton funname)

  let syscall
      (_ : L.i_loc)
      (lvs : int glvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (_ : int gexprs)
      (state : state) : state =
      state || memory_effect_lvs lvs

  let copn
      (_ : L.i_loc)
      (lvs : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (state : state) : state =
      state || memory_effect_lvs lvs
end

module MemoryEffectWalker = SimpleWalker (MemoryEffectMutator)

let memory_effects ((_, funcs) : ('info, 'asm) prog) : memory_effect Mf.t =
    List.fold_left
      (fun acc f ->
        let f, eff = MemoryEffectWalker.walk_func f None in
        Mf.add f.f_name eff acc )
      Mf.empty funcs
