open Jasmin
open Prog
open Utils
open State_walker
open Mutparameffect
open Wsize

let is_mutable_ptr var =
    match var.v_kind with
    | Reg (_, Pointer _)
     |Stack (Pointer _) ->
        true
    | _ -> false

let effect_of_lv (lv : int glval) (mut_params : Sv.t) : mut_param_effect =
    match lv with
    | Lmem (_, _, var, _)
     |Laset (_, _, _, var, _)
     |Lasub (_, _, _, var, _)
     |Lvar var ->
        if Sv.mem (L.unloc var) mut_params then
          Some
        else
          None
    | _ -> None

let variable_of_arg (expr : int gexpr) (mut_params : Sv.t) : var option =
    match expr with
    | Pvar v ->
        if Sv.mem (L.unloc v.gv) mut_params then
          Some (L.unloc v.gv)
        else
          None
    | _ -> None

let state_function_arg
    (state : mut_param_effect Mv.t)
    (funname : funname)
    (index : int)
    (expr : int gexpr)
    (mut_params : Sv.t)
    (function_params_map : int gvar list Mf.t) : mut_param_effect Mv.t =
    let expr_var = variable_of_arg expr mut_params in
    match expr_var with
    | None -> state
    | Some var ->
        let function_args = Mf.find funname function_params_map in
        let arg = List.nth function_args index in
        Mv.modify_def None var (fun s -> s || Depends (Sv.singleton arg)) state

let state_lv (state : mut_param_effect Mv.t) (lv : int glval) (mut_params : Sv.t) :
    mut_param_effect Mv.t =
    match lv with
    | Lvar var
     |Lmem (_, _, var, _)
     |Laset (_, _, _, var, _)
     |Lasub (_, _, _, var, _) ->
        Mv.modify_def None (L.unloc var) (fun s -> s || effect_of_lv lv mut_params) state
    | Lnone _ -> state

let build_mutparam_mutator ((_, funcs) : ('info, 'asm) Prog.prog) =
    let mutable_param_set =
        List.fold
          (fun acc func ->
            let mutparams =
                List.fold
                  (fun acc' var ->
                    if is_mutable_ptr var then
                      Sv.add var acc'
                    else
                      acc' )
                  Sv.empty func.f_args
            in
            Sv.union mutparams acc )
          Sv.empty funcs
    in
    let function_params_map =
        List.fold (fun acc func -> Mf.add func.f_name func.f_args acc) Mf.empty funcs
    in
    let module MutParamMutator : SimpleMutator with type state = mut_param_effect Mv.t = struct
      type state = mut_param_effect Mv.t

      let loop_stop_condition (_ : state) (_ : state) : bool = true

      let merge (_ : L.i_loc) (m1 : state) (m2 : state) : state =
          Mv.union (fun _ effect1 effect2 -> Some (effect1 || effect2)) m1 m2

      let merge_out_loop (s1 : state) (s2 : state) : state = merge L.i_dummy s1 s2

      let cond (_ : L.i_loc) (_ : int gexpr) (state : state) : state = state

      let cif (_ : L.i_loc) (_ : int gexpr) (state : state) : state * state = (state, state)

      let cassign
          (_ : L.i_loc)
          (lv : int glval)
          (_ : E.assgn_tag)
          (_ : int gty)
          (_ : int gexpr)
          (state : state) : state =
          state_lv state lv mutable_param_set

      let fcall
          (_ : L.i_loc)
          (lvs : int glvals)
          (fname : funname)
          (exprs : int gexprs)
          (state : state) : state =
          List.fold_lefti
            (fun acc i (lv, expr) ->
              let acc = state_function_arg acc fname i expr mutable_param_set function_params_map in
              state_lv acc lv mutable_param_set )
            state (List.combine lvs exprs)

      let syscall
          (_ : L.i_loc)
          (lvs : int glvals)
          (_ : BinNums.positive Syscall_t.syscall_t)
          (_ : int gexprs)
          (state : state) : state =
          List.fold (fun acc lv -> state_lv acc lv mutable_param_set) state lvs

      let copn
          (_ : L.i_loc)
          (lvs : int glvals)
          (_ : E.assgn_tag)
          (_ : 'asm Sopn.sopn)
          (_ : int gexprs)
          (state : state) : state =
          List.fold (fun acc lv -> state_lv acc lv mutable_param_set) state lvs
    end in
    (* let module MutParamWalker : SimpleWalker = SimpleWalker ((
         MutParamMutator : SimpleMutator with type state = mut_param_effect Mv.t ))
       in
       (module MutParamWalker) *)
    (module MutParamMutator : SimpleMutator with type state = mut_param_effect Mv.t)

let build_walker (module Mutator : SimpleMutator with type state = mut_param_effect Mv.t) =
    let module Walker = SimpleWalker.Make (Mutator) in
    (module Walker : SimpleWalker.S with type state = Mutator.state)

let build prog =
    let mutator = build_mutparam_mutator prog in
    let walker = build_walker mutator in
    walker
