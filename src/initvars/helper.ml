open Jasmin
open Prog
open Rd.Domain
open Rd.Srdi
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

let check_iv_error (data : iv_data) (domain : Domain.t) (var : var_i) : iv_data =
    match (L.unloc var).v_ty with
    | Arr _ -> data
    | _ ->
        if Sv.mem (L.unloc var) data.locals then
          match Mv.find_opt (L.unloc var) domain with
          | None ->
              Format.printf "Var %s not found in domain\n" (L.unloc var).v_name ;
              assert false (*This case is not possible with current version*)
          | Some iset ->
          match data.mode with
          | Strict ->
              if Srdi.mem Default iset then
                {data with errors= (L.loc var, VarNotIntialized (L.unloc var)) :: data.errors}
              else
                data
          | NotStrict ->
              if Srdi.equal iset (Srdi.singleton Default) then
                {data with errors= (L.loc var, VarNotIntialized (L.unloc var)) :: data.errors}
              else
                data
        else
          data

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
