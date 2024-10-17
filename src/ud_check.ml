open Jasmin

open Utils
open Prog
open Decoratum



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


let check_ud_var (dom:Domain.t) (_:L.i_loc) (modified_local_var: var): unit =
  match Mv.find_opt modified_local_var dom with 
  | None -> assert false (*TODO : handle error here*)
  | Some iset -> 
    if Si.is_empty iset then (
      assert false (*TODO : handle error here*)
    )


let ud_expr (dom:Domain.t) (instr:L.i_loc) (locvars:Sv.t) (e: expr) : unit =
  let used_locals_var: Sv.t= Sv.inter (Prog.vars_e e) (locvars) in
  Sv.iter (check_ud_var dom instr) used_locals_var

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
