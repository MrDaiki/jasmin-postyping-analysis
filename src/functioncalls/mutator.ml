open Jasmin
open Prog
open Walker
open FInfo
open Warning

module FCallMutator : Statemutator with type state = Sf.t = struct
  type state = Sf.t

  type annotation = unit

  let loop_stop_condition _ _ = true

  let cif (_ : int gexpr) (_ : L.i_loc) (env_th : state) (env_el : state) : state * annotation =
      (Sf.union env_th env_el, ())

  let cfor (_ : 'len gvar_i) (_ : 'len grange) (_ : L.i_loc) (_ : state) (env : state) :
      state * annotation =
      (env, ())

  let cfor_decl (_ : int gvar_i) (_ : 'len grange) (_ : L.i_loc) (env : state) : state = env

  let cwhile (_ : E.align) (_ : 'len gexpr) (_ : L.i_loc) (env1 : state) (env2 : state) :
      state * annotation =
      (Sf.union env1 env2, ())

  let cassign
      (_ : int glval)
      (_ : E.assgn_tag)
      (_ : 'len gty)
      (_ : 'len gexpr)
      (_ : L.i_loc)
      (state : state) : state * annotation =
      (state, ())

  let fcall _ fname _ _ d : state * annotation = (Sf.add fname d, ())

  let syscall
      (_ : int glvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (_ : 'len gexprs)
      (_ : L.i_loc)
      (state : state) : state * annotation =
      (state, ())

  let copn
      (_ : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (_ : L.i_loc)
      (state : state) : state * annotation =
      (state, ())
end

module FCallWalker = TreeWalker (FCallMutator)

let non_export_functions (funcs : ('len, 'info, 'asm) gfunc list) =
    List.fold_left
      (fun acc f ->
        if is_export f.f_cc then
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
    let diff = Sf.diff funcs_calls non_export_functions in
    Sf.iter (fun f -> pp_unf_warning Format.std_formatter (UnusedFunction f)) diff
