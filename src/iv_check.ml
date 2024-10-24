open Jasmin 

open Prog
open Utils
open Syscall_t
let name_of_syscall sc = 
  match sc with 
  | RandomBytes _ -> "randombytes"

(* representation of a variable/expression resolution time : 
Static : at compile time 
Dynamic : at runtime
*)
type resolution = Dynamic | Static

let (>) r1 r2 = 
  match r1,r2 with 
  | Dynamic,Static -> true
  | _ -> false
let (<) r1 r2 = 
  match r1,r2 with 
  | Static,Dynamic -> true
  | _ -> false

let v_kind_to_string (vk:Wsize.v_kind) = 
  match vk with 
  | Const -> "Const"
  | Stack _ -> "Stack"
  | Reg _ -> "Reg"
  | Inline  -> "Inline"
  | Global -> "Global"

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

let lval_static_vars (lvs: int glvals) : var list = 
  Sv.to_list (List.fold (fun s (lv: int glval) -> 
    match lv with 
    | Lnone _ -> s
    | Lvar v
    | Lmem (_,_,v,_) 
    | Laset (_,_,_,v,_)
    | Lasub (_,_,_,v,_)
    -> if (gvar_resolution v) = Static then (Sv.add (L.unloc v) s) else s
  ) Sv.empty lvs)
let expr_dynamic_vars expr : var list = 

  let rec sub_dynamics expr = 
    match expr with 
    | Pconst _ -> Sv.empty
    | Pbool _ -> Sv.empty
    | Parr_init _ -> Sv.empty
    | Pvar v -> if (gvar_resolution v.gv) = Dynamic then (Sv.singleton (L.unloc v.gv)) else Sv.empty
    | Pget (_,_,_,{gv=v;_},e)| Psub(_,_,_,{gv=v;_},e)|Pload (_,_,v,e)  -> 
      let s = sub_dynamics e in
      if (gvar_resolution v) = Dynamic then (Sv.add (L.unloc v) s) else s
    | Papp1 (_,e) -> 
      sub_dynamics e
    | Papp2 (_,e1,e2) -> 
      Sv.union (sub_dynamics e1) (sub_dynamics e2)
    | PappN (_,es) -> 
      List.fold (fun s e -> Sv.union s (sub_dynamics e)) Sv.empty es
    | Pif (_,e1,e2,e3) -> 
      Sv.union (sub_dynamics e1) (Sv.union (sub_dynamics e2) (sub_dynamics e3))
    in 
    Sv.to_list (sub_dynamics expr)

type 'len iv_error = 
| StaticToDynamicAssign of 'len glval * expr
| AccessToStaticWithDynamic of 'len gvar * expr
| StaticArgumentExpected of funname * 'len gvar * expr
| DynamicReturntoStatic of funname * 'len glval
| SyscallToStatic of 'len glvals *  string

let pp_iverror fmt error =
  match error with 
  | StaticToDynamicAssign (lv,expr) -> 
    let vars = expr_dynamic_vars expr in
    let v_name = match lv with 
      | Lnone _ -> assert false
      | Lvar v -> (L.unloc v).v_name
      | Lmem (_,_,v,_) 
      | Laset (_,_,_,v,_)
      | Lasub (_,_,_,v,_)
      -> (L.unloc v).v_name
    in
    Format.fprintf fmt "Trying to assign inline variable %s with dynamic expression %a@.The expression is dynamic becauses of variables : %a" 
    v_name 
    (Printer.pp_expr~debug:(false)) expr
    (Utils.pp_list ", " (Printer.pp_var ~debug:(false))) vars
  | AccessToStaticWithDynamic (v,expr) ->
    let vars = expr_dynamic_vars expr in
    Format.fprintf fmt "Trying to access inline array %s with dynamic variables : %a" 
    v.v_name 
    (Utils.pp_list ", " (Printer.pp_var ~debug:(false))) vars
  | StaticArgumentExpected (f, v, expr) -> 
    let vars = expr_dynamic_vars expr in
    Format.fprintf fmt "In function `%s` call, argument `%s` is inline, but dynamic variables (%a) were passed" 
    f.fn_name
    v.v_name 
    (Utils.pp_list ", " (Printer.pp_var ~debug:(false))) vars
  | DynamicReturntoStatic (f,lv) ->
    Format.fprintf fmt "In function `%s` call, return value is dynamic, but it is assigned to inline variable `%s`" 
    f.fn_name
    (match lv with 
      | Lnone _ -> assert false
      | Lvar v -> (L.unloc v).v_name
      | Lmem (_,_,v,_) 
      | Laset (_,_,_,v,_)
      | Lasub (_,_,_,v,_)
      -> (L.unloc v).v_name
    )
  | SyscallToStatic (lvs,syscall) -> 
    let static_vars = lval_static_vars lvs in
    Format.fprintf fmt "Syscall `#%s`return value is dynamic, but it is assigned to static variables : %a"
    syscall
    (Utils.pp_list ", " (Printer.pp_var ~debug:(false))) static_vars

exception IVError of L.t * int iv_error

let iverror ~loc (code:int iv_error) = 
  IVError (loc,code)

let rs_iverror ~loc (code:int iv_error) = 
  raise (iverror ~loc code)



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
    rs_iverror ~loc:(L.loc var) (AccessToStaticWithDynamic ((L.unloc var),expr))
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
    args : int gvar list;
    resolution : resolution
  }
  
  type t = func_sig Mf.t

  let empty = Mf.empty

  let add (env:t) (f:('info,'asm) func)  : t = 
    let return_signature = List.map (fun v -> gvar_resolution v) f.f_ret in
    let args_signature = List.map (fun v -> var_resolution v) f.f_args in
    let args = f.f_args in
    let resolution = match f.f_cc with 
      | Internal -> Static
      | _ -> Dynamic
    in 
    Mf.add f.f_name {return_signature;args_signature;args;resolution} env
  
  let get (env:t) (f:funname)  : func_sig = 
    Mf.find f env  

  let check_arguments loc (env:t) (f:funname) (args: 'len gexpr list): unit = 
    let f_sig = get env f in
    let def_sig = f_sig.args_signature in
    let arg_sig = List.map (fun e -> expr_resolution e) args in
    let zip = List.combine (List.combine def_sig arg_sig) (List.combine f_sig.args args) in
    List.iter (
      fun ((d,a),(var,expr)) -> 
        if d < a then 
          rs_iverror ~loc:(loc) (StaticArgumentExpected (f,var,expr))
    ) zip

  let check_return loc (env:t) (f:funname) (lvs: 'len glval list) : unit = 
    let ret_sig = (get env f).return_signature in
    let lv_sig = List.map (fun lv -> lv_resolution lv) lvs in
    let zip = List.combine (List.combine ret_sig lv_sig) lvs in
    List.iter (
      fun ((r,l),lv) -> 
        if l < r then 
          rs_iverror ~loc:(loc) (DynamicReturntoStatic (f,lv))
    ) zip

end

let iv_assign loc lv expr = 
  let lv_res = lv_resolution lv in
  let expr_res = expr_resolution expr in
  if expr_res > lv_res then 
    rs_iverror ~loc:loc (StaticToDynamicAssign (lv, expr))
  else
    inf_resolution [lv_res]


let rec iv_instr (function_env) (instr) : resolution = 
  let loc = (instr.i_loc.base_loc) in
  match instr.i_desc with 
  | Cassgn (lv,_,_,expr) -> 
    iv_assign loc lv expr
  | Copn (lvs,_,_,exprs) -> 
    let zip = List.combine lvs exprs in
    List.fold_left (fun min (lv,expr) -> min_resolution (iv_assign loc lv expr) min) Dynamic zip
  | Ccall (lvs,fn,exprs)->
    FunctionResolution.check_arguments loc function_env fn exprs;
    FunctionResolution.check_return loc function_env fn lvs;
    inf_resolution (List.map (fun lv -> lv_resolution lv) lvs)
  | Csyscall (lvs,sc,_) -> 
    let lv_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
    if lv_res = Static then 
      let sc_name = name_of_syscall sc in
      rs_iverror  ~loc:loc (SyscallToStatic (lvs, sc_name))
    else 
      Dynamic
  | Cif (c,b1,b2) -> 
    let c_res = expr_resolution c in
    let b1_res = iv_stmt function_env b1 in
    let b2_res = iv_stmt function_env b2 in 
    if c_res > b1_res || c_res > b2_res then 
      assert false (*Error : if one of  the bloc is static, then the condition and the other bloc must also be static*)
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