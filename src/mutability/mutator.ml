open Jasmin
open Prog
open Utils
open Error
open Environment
open State_walker

let md_lvalue (variables : MutabilityEnvironment.t) (lv : 'len glval) : unit =
    match lv with
    | Lnone _ -> ()
    | Lmem (_, _, gv, _)
     |Laset (_, _, _, gv, _)
     |Lasub (_, _, _, gv, _)
     |Lvar gv ->
        if not (MutabilityEnvironment.check_mutable variables (L.unloc gv)) then
          rs_mderror ~loc:(L.loc gv) (AssignOnNonMutablePtr (L.unloc gv).v_name)

let md_lvalues (variables : MutabilityEnvironment.t) (lv : 'len glval list) : unit =
    List.iter (md_lvalue variables) lv

let md_func_args (variables : MutabilityEnvironment.t) (fun_name : funname) (args : 'len gexpr list)
    : unit =
    let rec cmp_mutability (fsig : (mutability * string) list) (args : expr list) : unit =
        match (fsig, args) with
        | (NotPtr, _) :: s, _ :: a -> cmp_mutability s a
        | (Mutable, param) :: s, arg :: a -> (
            match arg with
            | Pvar gv ->
                if not (MutabilityEnvironment.check_mutable variables (L.unloc gv.gv)) then
                  rs_mderror ~loc:(L.loc gv.gv)
                    (FunctionArgNotMutable (fun_name, param, (L.unloc gv.gv).v_name))
                else
                  cmp_mutability s a
            | _ -> assert false )
        | (Constant, _) :: s, _ :: a -> cmp_mutability s a
        | [], [] -> ()
        | _, []
         |[], _ ->
            assert false
        (*This error shoud never be raised, it correspond to call with the wrong number of argument, which is checked previously*)
    in
    cmp_mutability (Mf.find fun_name variables.functions) args

module MutabilityMutator : SimpleMutator with type state = MutabilityEnvironment.t = struct
  type state = MutabilityEnvironment.t

  let loop_stop_condition (_ : state) (_ : state) : bool = true

  let merge_out_loop (s1 : state) (s2 : state) : state = MutabilityEnvironment.merge s1 s2

  let merge (_ : L.i_loc) (s1 : state) (s2 : state) : state = MutabilityEnvironment.merge s1 s2

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

  let fcall (_ : L.i_loc) (_ : int glvals) (_ : funname) (_ : int gexprs) (state : state) : state =
      state

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
  (* let loop_stop_condition _ _ = true

     let cif
         (_ : int gexpr)
         (_ : L.i_loc)
         (env_th : MutabilityEnvironment.t)
         (env_el : MutabilityEnvironment.t) : state * annotation =
         (MutabilityEnvironment.merge env_th env_el, ())

     let cfor (_ : 'len gvar_i) (_ : 'len grange) (_ : L.i_loc) (_ : state) (env : state) :
         state * annotation =
         (env, ())

     let cfor_decl (_ : int gvar_i) (_ : 'len grange) (_ : L.i_loc) (env : state) : state = env

     let cwhile (_ : E.align) (_ : 'len gexpr) (_ : L.i_loc) (_ : state) (env : state) :
         state * annotation =
         (env, ())

     let cassign
         (lv : int glval)
         (_ : E.assgn_tag)
         (_ : 'len gty)
         (_ : 'len gexpr)
         (_ : L.i_loc)
         (state : state) : state * annotation =
         md_lvalue state lv ; (state, ())

     let fcall (lvs : int glvals) (fn : funname) (exprs : int gexprs) (_ : L.i_loc) (state : state) :
         state * annotation =
         md_lvalues state lvs ; md_func_args state fn exprs ; (state, ())

     let syscall
         (lvs : int glvals)
         (_ : BinNums.positive Syscall_t.syscall_t)
         (_ : 'len gexprs)
         (_ : L.i_loc)
         (state : state) : state * annotation =
         md_lvalues state lvs ; (state, ())

     let copn
         (lvs : int glvals)
         (_ : E.assgn_tag)
         (_ : 'asm Sopn.sopn)
         (_ : int gexprs)
         (_ : L.i_loc)
         (state : state) : state * annotation =
         md_lvalues state lvs ; (state, ()) *)
end

module MutabilityWalker = SimpleWalker.Make (MutabilityMutator)

let populate_environment_globals (globs : global_decl list) =
    List.fold
      (fun env (v, _) -> MutabilityEnvironment.add_variable env v)
      MutabilityEnvironment.empty globs

let populate_environment_funcs (funcs : ('len, 'info, 'asm) gfunc list) env =
    List.fold
      (fun env func -> MutabilityEnvironment.add_function_arguments env func.f_name func.f_args)
      env funcs

let populate_env (globs, funcs) =
    let env = populate_environment_globals globs in
    populate_environment_funcs funcs env

let md_func (env : MutabilityEnvironment.t) (f : ('info, 'asm) func) =
    let env = List.fold (fun v g -> MutabilityEnvironment.add_variable v g) env f.f_args in
    MutabilityWalker.walk_func f env

let md_prog ((globs, funcs) : ('info, 'asm) prog) =
    let env = populate_env (globs, funcs) in
    List.iter
      (fun f ->
        let _ = md_func env f in
        () )
      funcs
