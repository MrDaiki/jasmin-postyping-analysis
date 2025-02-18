open Jasmin
open Prog

type mutability =
| Constant
| Mutable
| NotArrayPtr

let var_mutability (v : var) : mutability =
    match v.v_kind with
    | Const
     |Inline
     |Global ->
        NotArrayPtr
    | Stack ref
     |Reg (_, ref) ->
    match ref with
    | Direct -> NotArrayPtr
    | Pointer wr ->
    match wr with
    | Constant -> Constant
    | Writable -> Mutable

module FunctionsArguments = struct
  type t = var list Mf.t

  let empty : t = Mf.empty

  let add_function (f : ('info, 'asm) func) (env : t) : t = Mf.add f.f_name f.f_args env

  let get_argument_mutability (f : funname) (arg_index : int) (fargs : t) : mutability =
      match Mf.find_opt f fargs with
      | None -> assert false
      | Some args ->
      match List.nth_opt args arg_index with
      | None -> assert false
      | Some arg -> var_mutability arg
end

let print_mutability (m : mutability) =
    match m with
    | Constant -> Format.printf "Constant\n"
    | Mutable -> Format.printf "Mutable\n"
    | NotArrayPtr -> Format.printf "NotArrayPtr\n"
