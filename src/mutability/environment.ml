open Jasmin
open Prog
open Utils

type mutability =
| Constant
| Mutable
| NotPtr

module MutabilityEnvironment = struct
  type t =
  { mutable_vars: Sv.t
  ; functions: (mutability * string) list Mf.t }

  let empty = {mutable_vars= Sv.empty; functions= Mf.empty}

  let merge ({mutable_vars; functions} : t) (env2 : t) : t =
      {mutable_vars= Sv.union mutable_vars env2.mutable_vars; functions}

  let add_mutable (vars : t) (v : var) : t = {vars with mutable_vars= Sv.add v vars.mutable_vars}

  let add_variable (variables : t) (v : int gvar) : t =
      match v.v_kind with
      | Reg (_, wr)
       |Stack wr -> (
          match wr with
          | Direct -> variables
          | Pointer wt ->
          match wt with
          | Constant -> variables
          | Writable -> add_mutable variables v )
      | _ -> variables

  let add_function_arguments (vars : t) (f : funname) (args : 'len gvar list) : t =
      let args_mut =
          List.map
            (fun gv ->
              match gv.v_kind with
              | Reg (_, wr)
               |Stack wr -> (
                  match wr with
                  | Direct -> (NotPtr, gv.v_name)
                  | Pointer wt ->
                  match wt with
                  | Constant -> (Constant, gv.v_name)
                  | Writable -> (Mutable, gv.v_name) )
              | _ -> (NotPtr, gv.v_name) )
            args
      in
      {vars with functions= Mf.add f args_mut vars.functions}

  let check_mutable (vars : t) (v : var) : bool = Sv.mem v vars.mutable_vars
end
