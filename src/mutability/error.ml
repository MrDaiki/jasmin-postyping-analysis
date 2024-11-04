open Jasmin
open Prog

type md_error =
| AssignOnNonMutablePtr of string
| FunctionArgNotMutable of funname * string * string

let pp_mderror fmt (error : md_error) =
    match error with
    | AssignOnNonMutablePtr id ->
        Format.fprintf fmt "Trying to mutate %s, which is a non mutable pointer" id
    | FunctionArgNotMutable (fn, id_arg, id_var) ->
        Format.fprintf fmt
          "in function %s call, argument %s is expected to be mutable, but argument %s is not \
           mutable"
          fn.fn_name id_arg id_var

exception MdError of L.t * md_error

let mderror ~loc (code : md_error) = MdError (loc, code)

let rs_mderror ~loc (code : md_error) = raise (mderror ~loc code)
