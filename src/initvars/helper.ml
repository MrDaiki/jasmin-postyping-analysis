open Jasmin
open Prog
open Rd
open Types
open UvError

type check_mode =
| Strict
| NotStrict

type iv_data =
{ locals: Sv.t
; mode: check_mode
; errors: (Location.t * uv_error) list }

let is_local (v : int ggvar) =
    match v.gs with
    | Slocal -> true
    | _ -> false

let _inner_check_iv_error data domain loc var =
    if Sv.mem var data.locals then
      match Mv.find_opt var domain with
      | None ->
          Format.printf "Var %s not found in domain\n" var.v_name ;
          assert false (*This case is not possible with current version*)
      | Some iset ->
      match data.mode with
      | Strict ->
          if SIloc.mem Default iset then
            {data with errors= (loc, VarNotIntialized var) :: data.errors}
          else
            data
      | NotStrict ->
          if SIloc.equal iset (SIloc.singleton Default) then
            {data with errors= (loc, VarNotIntialized var) :: data.errors}
          else
            data
    else
      data

let check_iv_error (data : iv_data) (domain : Domain.t) (var : var_i) : iv_data =
    let loc, var = (L.loc var, L.unloc var) in
    match var.v_ty with
    | Arr _ -> (
        match var.v_kind with
        | Stack Direct
         |Reg (_, Direct) ->
            data
        | _ -> _inner_check_iv_error data domain loc var )
    | _ -> _inner_check_iv_error data domain loc var

let rec check_iv_expr (data : iv_data) (domain : Domain.t) (expr : expr) : iv_data =
    match expr with
    | Pconst _ -> data
    | Pbool _ -> data
    | Parr_init _ -> data
    | Pvar var ->
        if Sv.mem (L.unloc var.gv) data.locals then
          check_iv_error data domain var.gv
        else
          data
    | Pget (_, _, _, var, expr) ->
        let data = check_iv_expr data domain expr in
        check_iv_error data domain var.gv
    | Psub (_, _, _, var, expr) ->
        let data = check_iv_expr data domain expr in
        check_iv_error data domain var.gv
    | Pload (_, _, var, expr) ->
        let data = check_iv_expr data domain expr in
        check_iv_error data domain var
    | Papp1 (_, expr) -> check_iv_expr data domain expr
    | Papp2 (_, l, r) -> check_iv_expr (check_iv_expr data domain l) domain r
    | PappN (_, exprs) -> List.fold_left (fun d e -> check_iv_expr d domain e) data exprs
    | Pif (_, e1, e2, e3) ->
        let data = check_iv_expr data domain e1 in
        let data = check_iv_expr data domain e2 in
        check_iv_expr data domain e3
