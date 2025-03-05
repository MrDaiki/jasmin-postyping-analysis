open Jasmin
open Prog
open Rd
open Types
open InitVarError
open Error.CompileError

type check_mode =
| Strict
| NotStrict

type iv_data =
{ locals: Sv.t
; mode: check_mode
; errors: compile_error list }

let is_local (v : int ggvar) =
    match v.gs with
    | Slocal -> true
    | _ -> false

let _inner_check_iv_error data domain loc var =
    if Sv.mem var data.locals then
      match Mv.find_opt var domain with
      | None -> assert false (*This case is not possible with current version*)
      | Some iset ->
      match data.mode with
      | Strict ->
          if SIloc.mem Default iset then
            {data with errors= create_init_var_error var loc :: data.errors}
          else
            data
      | NotStrict ->
          if SIloc.equal iset (SIloc.singleton Default) then
            {data with errors= create_init_var_error var loc :: data.errors}
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

let check_iv_lv (data : iv_data) (domain : Domain.t) (lv : lval) : iv_data =
    match lv with
    | Lnone _
     |Lvar _ ->
        data
    | Lmem (_, _, gv, expr)
     |Laset (_, _, _, gv, expr)
     |Lasub (_, _, _, gv, expr) ->
        let data = check_iv_expr data domain expr in
        check_iv_error data domain gv

let check_iv_lvs (data : iv_data) (domain : Domain.t) (lvs : lval list) : iv_data =
    List.fold_left (fun d lv -> check_iv_lv d domain lv) data lvs
