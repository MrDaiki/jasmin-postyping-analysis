open Jasmin
open Prog

type unf_warning = UnusedFunction of funname

let pp_unf_warning fmt warning =
    match warning with
    | UnusedFunction funame ->
        Format.fprintf fmt "Function '%s' is never called and is not an export function"
          funame.fn_name
