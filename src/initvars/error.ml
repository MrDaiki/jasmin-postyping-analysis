open Jasmin
open Prog

type uv_error = VarNotIntialized of var

let pp_uderror fmt error =
    match error with
    | VarNotIntialized v ->
        Format.fprintf fmt "Variable '%s' (declared at : %s) not initialized" v.v_name
          (L.tostring v.v_dloc)

exception UvError of L.t * uv_error

let uderror ~loc (code : uv_error) = UvError (loc, code)

let rs_uderror ~loc (code : uv_error) = raise (uderror ~loc code)
