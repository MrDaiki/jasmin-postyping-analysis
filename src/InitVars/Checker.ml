open Jasmin
open Prog
open Rd
open Types
open InitVarError
open Error.CompileError

(**
Check mode for initialised variable analysis 
- Strict : Check if a path exists where variable may not be initialised (can trigger false positive)
- NotStrict : Check if there is no path in the program where variable is initialised (less restrictive but let some error pass)
*)
type check_mode =
| Strict
| NotStrict

(**
Visitor inner state. 
locals : Sv.t local variables of the analysed program
mode : check_mode mode of the analysis
errors : list of error found during analysis
*)
type iv_data =
{ mode: check_mode
; errors: compile_error list }

(*
Check if a variable is local
args : int ggvar 
return : bool (true if variable is local, false otherwise)
*)
let is_local (v : int ggvar) =
    match v.gs with
    | Slocal -> true
    | _ -> false

let is_local_gv (v : var) = not (Jasmin.Prog.V.is_glob v)

(*
Error checking function of the analysis. Check if variable `var` passed as argument is initialised in the current domain. It also check if the variable is local (because initialisation problem only make sense for local variables).
args :
- data : iv_data state of the visitor
- domain : Domain.t In domain of the reaching definition analysis for current instruction
- loc : Jasmin.Location.t location of the variable
- var : var variable to check
return : iv_data (updated the list of error if `var` is not initialised)
*)
let _inner_check_iv_error (data : iv_data) (domain : Domain.t) (loc : Jasmin.Location.t) (var : var)
    =
    if is_local_gv var then
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

(**
Pre error checking function. It is used to check if variable `var` is an non ptr array (because non ptr array are by default initialised in Jasmin specification).
args : 
- data : iv_data state of the visitor
- domain : Domain.t In domain of the reaching definition analysis for current instruction
- var : var_i variable to check
return : iv_data (updated state)
*)
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

module InitVarCheckerLogic :
  Visitor.ExpressionChecker.ExpressionCheckerLogic
    with type domain = Domain.t
     and type self = iv_data = struct
  type domain = Domain.t

  type self = iv_data

  (**
  Check variable initialisation for expressions in the program
  args : 
  - self : (iv_data) state of the visitor 
  - domain : (Domain.t) In domain of the reaching definition analysis for current instruction
  - expr : (expr) checked expression
  *)
  let rec check_expr
      (self : self)
      (domain : domain)
      (loc : Jasmin.Location.i_loc)
      (expr : Jasmin.Prog.expr) : self =
      match expr with
      | Pconst _ -> self
      | Pbool _ -> self
      | Parr_init _ -> self
      | Pvar var -> check_iv_error self domain var.gv
      | Pget (_, _, _, var, expr) ->
          let self = check_expr self domain loc expr in
          check_iv_error self domain var.gv
      | Psub (_, _, _, var, expr) ->
          let self = check_expr self domain loc expr in
          check_iv_error self domain var.gv
      | Pload (_, _, var, expr) ->
          let self = check_expr self domain loc expr in
          check_iv_error self domain var
      | Papp1 (_, expr) -> check_expr self domain loc expr
      | Papp2 (_, l, r) -> check_expr (check_expr self domain loc l) domain loc r
      | PappN (_, exprs) -> List.fold_left (fun d e -> check_expr d domain loc e) self exprs
      | Pif (_, e1, e2, e3) ->
          let self = check_expr self domain loc e1 in
          let self = check_expr self domain loc e2 in
          check_expr self domain loc e3

  let check_return_variable (self : self) (domain : domain) (var : Jasmin.Prog.var_i) : self =
      check_iv_error self domain var

  (** 
  Check for variable initialisation in left values. It apply initialisation check for expressions in left values (array access, slice, memory access).
    args :
    - self : iv_data state of the visitor
    - domain : Domain.t In domain of the reaching definition analysis for current instruction
    - lv : lval left value to check
    return : iv_data (updated state)
  *)
  let check_lv_variable (self : self) (domain : domain) (var : Jasmin.Prog.var_i) : self =
      check_iv_error self domain var
end
