open Jasmin 

open Prog


(* representation of a variable/expression resolution time : 
Static : at compile time 
Dynamic : at runtime
*)
type resolution = Dynamic | Static

let (<) r1 r2 = 
  match r1,r2 with 
  | Static,Dynamic -> true
  | _ -> false


let min_resolution (r1:resolution) (r2:resolution) : resolution = 
  if r1 < r2 then r1 else r2
  
let sup_resolution (r:resolution list) = 
  List.fold_left (fun res r -> if r > res then r else res) Static r

let inf_resolution (r:resolution list) = 
  List.fold_left (fun res r -> if r < res then r else res) Dynamic r

let var_resolution (v:'len gvar) : resolution = 
  match v.v_kind with 
  | Inline -> Static
  | _ -> Dynamic

let gvar_resolution (gv:'len gvar_i) : resolution =
  var_resolution (L.unloc gv)


(*
Compute the resolution time of an expression : if it contains a dynamic variable, then it must be dynamic
It also check coherence between array and content expression: if the array is static, then the expression accessing the array must be static
*)
let rec expr_resolution (expr:expr) : resolution = 
  match expr with 
  | Pconst _ -> Static
  | Pbool _ -> Static
  | Parr_init _ -> Static
  | Pvar v -> gvar_resolution v.gv
  | Pget (_,_,_,{gv=v;_},e)| Psub(_,_,_,{gv=v;_},e)|Pload (_,_,v,e)  -> 
    array_resolution_coherence v e
  | Papp1 (_,e) -> 
    expr_resolution e
  | Papp2 (_,e1,e2) -> 
    let e1_res = expr_resolution e1 in
    let e2_res = expr_resolution e2 in
    sup_resolution [e1_res;e2_res]
  | PappN (_,es) -> 
    sup_resolution (List.map expr_resolution es)
  | Pif (_,e1,e2,e3) -> 
    sup_resolution [expr_resolution e1;expr_resolution e2;expr_resolution e3]



and array_resolution_coherence var expr = 
  let var_res = gvar_resolution var in
  let expr_res = expr_resolution expr in
  if var_res < expr_res then 
    assert false (* Error : if lval is static, then the content of the call can't be dynamic *)
  else 
    var_res

let lv_resolution (lv: 'len glval) : resolution = 
  match lv with 
  | Lnone _ -> Static 
  | Lvar v -> gvar_resolution v
  | Lmem (_,_,v,e) 
  | Laset (_,_,_,v,e)
  | Lasub (_,_,_,v,e) 
  -> 
    array_resolution_coherence v e



module FunctionResolution = struct 
  
  type func_sig = {
    return_signature: resolution list;
    args_signature: resolution list;
    resolution : resolution
  }
  
  type t = func_sig Mf.t

  let empty = Mf.empty

  let add (env:t) (f:('info,'asm) func)  : t = 
    let return_signature = List.map (fun v -> gvar_resolution v) f.f_ret in
    let args_signature = List.map (fun v -> var_resolution v) f.f_args in
    let resolution = match f.f_cc with 
      | Internal -> Static
      | _ -> Dynamic
    in 
    Mf.add f.f_name {return_signature;args_signature;resolution} env
  
  let get (env:t) (f:funname)  : func_sig = 
    Mf.find f env  

  let check_arguments (env:t) (f:funname) (args: 'len gexpr list): unit = 
    let def_sig = (get env f).args_signature in
    let arg_sig = List.map (fun e -> expr_resolution e) args in
    let zip = List.combine (List.combine def_sig arg_sig) args in
    List.iter (
      fun ((d,a),_) -> 
        if d < a then 
          assert false (* Error : if definition resolution is static, then argument must be static*)
    ) zip

  let check_return (env:t) (f:funname) (lvs: 'len glval list) : unit = 
    let ret_sig = (get env f).return_signature in
    let lv_sig = List.map (fun lv -> lv_resolution lv) lvs in
    let zip = List.combine (List.combine ret_sig lv_sig) lvs in
    List.iter (
      fun ((r,l),_) -> 
        if l < r then 
          assert false (* if the return of a function is dynamic, we cannot put it in static variable *)
    ) zip

end

let iv_assign lv expr = 
  let lv_res = lv_resolution lv in
  let expr_res = expr_resolution expr in
  if expr_res > lv_res then 
    assert false 
  else
    inf_resolution [lv_res]

let rec iv_instr (function_env) (instr) : resolution = 
  match instr.i_desc with 
  | Cassgn (lv,_,_,expr) -> 
    iv_assign lv expr
  | Copn (lvs,_,_,exprs) -> 
    let zip = List.combine lvs exprs in
    List.fold_left (fun min (lv,expr) -> min_resolution (iv_assign lv expr) min) Dynamic zip
  | Ccall (lvs,fn,exprs)->
    FunctionResolution.check_arguments function_env fn exprs;
    FunctionResolution.check_return function_env fn lvs;
    inf_resolution (List.map (fun lv -> lv_resolution lv) lvs)
  | Csyscall (lvs,_,_) -> 
    let lv_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
    if lv_res = Static then 
      assert false (*Error : syscall are dynamic, so cannot be affected to static variables*)
    else 
      Dynamic
  | Cif (c,b1,b2) -> 
    let c_res = expr_resolution c in
    let b1_res = iv_stmt function_env b1 in
    let b2_res = iv_stmt function_env b2 in 
    if c_res > b1_res || c_res > b2_res then 
      assert false (*Error : if one the bloc is static, then the condition and the other bloc must also be static*)
    else 
    min_resolution b1_res b2_res
  | Cfor (v,(_,rl,rr),b) -> 
    let v_res = gvar_resolution v in 
    let b_res = iv_stmt function_env b in
    let rl_res = expr_resolution rl in
    let rr_res = expr_resolution rr in 
    if v_res < (min_resolution rl_res rr_res)
    then assert false (*Error : if the variable is static, then it's range bound must be static*)
    else
    if b_res < v_res then 
      assert false (*Error : if the bloc is static, then the variable must be static*)
    else 
      v_res
  | Cwhile (_,b1,c,b2) -> 
    let b1_res = iv_stmt function_env b1 in
    let c_res = expr_resolution c in 
    let b2_res = iv_stmt function_env b2 in

    if (c_res > (min_resolution b1_res b2_res)) 
    then assert false (* Error : if a bloc is static, then the condition must be static*)
    else 
      min_resolution b1_res b2_res

(* We consider that a bloc is static if at least one of its instruction affect a static variable*)
and iv_stmt (function_env:FunctionResolution.t) (stmt: ('len,'info,'asm) ginstr list) : resolution =
  List.fold_left (fun min i -> min_resolution (iv_instr function_env i) min) Dynamic stmt

let iv_func function_env funcs : unit = 
  let _ = iv_stmt function_env funcs.f_body in ()

let iv_prog ((_,funcs): ('info,'asm) prog) : unit = 
  let function_env = List.fold_left (fun env func -> FunctionResolution.add env func) FunctionResolution.empty funcs in
  List.iter (iv_func function_env) funcs 