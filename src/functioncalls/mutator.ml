open Jasmin
open Prog
open State_walker
open FInfo
open Warning

module FCallMutator : SimpleMutator with type state = Sf.t = struct
  type state = Sf.t

  let loop_stop_condition (_ : state) (_ : state) : bool = true

  let merge_out_loop (s1 : state) (s2 : state) : state = Sf.union s1 s2

  let merge (_ : L.i_loc) (s1 : state) (s2 : state) : state = Sf.union s1 s2

  let cond (_ : L.i_loc) (_ : int gexpr) (state : state) : state = state

  let cif (_ : L.i_loc) (_ : int gexpr) (state : state) : state * state = (state, state)

  let cassign
      (_ : L.i_loc)
      (_ : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (_ : int gexpr)
      (state : state) : state =
      state

  let fcall (_ : L.i_loc) (_ : int glvals) (funname : funname) (_ : int gexprs) (state : state) :
      state =
      Sf.add funname state

  let syscall
      (_ : L.i_loc)
      (_ : int glvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (_ : int gexprs)
      (state : state) : state =
      state

  let copn
      (_ : L.i_loc)
      (_ : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (state : state) : state =
      state
end

module FCallWalker = SimpleWalker.Make (FCallMutator)

let non_export_functions (funcs : ('len, 'info, 'asm) gfunc list) =
    List.fold_left
      (fun acc f ->
        if not (is_export f.f_cc) then
          Sf.add f.f_name acc
        else
          acc )
      Sf.empty funcs

let fc_prog ((_, funcs) : ('info, 'asm) prog) : unit =
    let funcs_calls =
        List.fold_left
          (fun acc f ->
            let _, f_calls = FCallWalker.walk_func f Sf.empty in
            Sf.union acc f_calls )
          Sf.empty funcs
    in
    let non_export_functions = non_export_functions funcs in
    let diff = Sf.diff non_export_functions funcs_calls in
    Sf.iter (fun f -> pp_unf_warning Format.std_formatter (UnusedFunction f)) diff
