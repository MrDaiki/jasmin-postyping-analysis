open Jasmin

open Utils
open Prog
open Decoratum


module LV = struct
  type t = (int) gvar_i
  let compare x y =
    let ux:CoreIdent.uid = (L.unloc x).v_id in 
    let uy:CoreIdent.uid = (L.unloc y).v_id in    
    Uint63.compares ux uy

end 

module Slv = Set.Make(LV)

type ud_error = 
| VarNotIntialized of ((int) gvar_i)


let pp_uderror fmt error =
  match error with 
  | VarNotIntialized v -> 
    let v = L.unloc v in
    Format.fprintf fmt "Variable '%s' (declared at : %s) not initialized" v.v_name (L.tostring v.v_dloc)

exception UdError of L.t * ud_error

let uderror ~loc (code:ud_error) = 
  UdError (loc,code)
let rs_uderror ~loc (code:ud_error) = 
  raise (uderror ~loc code)

let is_local (v:int ggvar) = 
  match v.gs with 
  | Slocal -> true
  | _ -> false
let rec lvars_e (e:expr) : Slv.t =
  match e with
  | Pconst _ -> Slv.empty
  | Pbool  _ -> Slv.empty
  | Parr_init _ -> Slv.empty
  | Pvar var -> 
    if is_local var 
    then Slv.singleton var.gv
    else Slv.empty
  | Pget  (_ , _ , _,var,expr) -> 
    let eset = lvars_e expr in
    if is_local var then Slv.add var.gv eset
    else 
      eset
  | Psub (_,_,_,var,expr) ->
    let eset = lvars_e expr in
    if is_local var then Slv.add var.gv eset
    else 
      eset
  | Pload (_,_,var,expr) -> 
    let eset = lvars_e expr in
    Slv.add var eset (*this var is always local*)
  | Papp1 (_,expr) -> (lvars_e expr) 
  | Papp2 (_,l,r) -> Slv.union (lvars_e l) (lvars_e r)
  | PappN (_,exprs) -> List.fold (fun sl e -> Slv.union sl (lvars_e e)) Slv.empty exprs
  | Pif (_,e1,e2,e3) -> Slv.union (lvars_e e1) (Slv.union (lvars_e e2) (lvars_e e3))


let check_ud_var (dom:Domain.t) (_:L.i_loc) (modified_local_var: 'len gvar_i): unit =
  match Mv.find_opt (L.unloc modified_local_var) dom with 
  | None -> 
    rs_uderror ~loc:(L.loc modified_local_var) (VarNotIntialized modified_local_var)
  | Some iset -> 
    if Si.is_empty iset then (
      assert false (*TODO : handle error here*)
    )


let ud_expr (dom:Domain.t) (instr:L.i_loc) (locvars:Sv.t) (e: expr) : unit =
  let found_vars = lvars_e e in
  let used_local_vars = Slv.fold (fun e sl -> if (Sv.mem (L.unloc e) locvars) then (Slv.add e sl) else sl) found_vars Slv.empty in
  Slv.iter (check_ud_var dom instr) used_local_vars

let ud_range (dom:Domain.t)  (locvars: Sv.t) (instr:L.i_loc) ((_,e1,e2): 'len grange) : unit =
  ud_expr dom instr locvars e1;
  ud_expr dom instr locvars e2


let rec ud_instr (locvars: Sv.t) (instr: ('len,Domain.t,'asm) ginstr)  : unit =
  match instr.i_desc with
  | Cassgn (_,_,_,expr) -> 
    ud_expr (instr.i_info) instr.i_loc locvars expr
  | Copn (_,_,_,exprs) -> 
    List.iter (ud_expr (instr.i_info) instr.i_loc locvars) exprs
  | Csyscall (_,_,exprs) ->
    List.iter (ud_expr (instr.i_info) instr.i_loc locvars) exprs
  |  Ccall (_,_,exprs) -> 
    List.iter (ud_expr (instr.i_info) instr.i_loc locvars) exprs
  | Cif (expr,b1,b2) ->
    ud_expr (instr.i_info) instr.i_loc locvars expr;
    ud_stmt locvars b1;
    ud_stmt locvars b2
  | Cfor (_,r,b) ->
    ud_range (instr.i_info) locvars instr.i_loc r;
    ud_stmt locvars b
  | Cwhile (_,b1,c,b2) ->
    ud_expr (instr.i_info) instr.i_loc locvars c;
    ud_stmt locvars b1;
    ud_stmt locvars b2

and ud_stmt (locvars: Sv.t) stmt : unit =
  List.iter (ud_instr locvars) stmt

let ud_func (f: ('info','asm) func) : unit =
  let _,func_body= rd_fundef f in
  let locvars = Prog.locals f in 
  ud_stmt locvars func_body 

let ud_prog ((_,funcs): ('info','asm) prog): unit =
  List.iter ud_func funcs
