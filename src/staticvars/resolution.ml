open Jasmin
open Prog
open Utils
open Error
open Types.Sgv

type resolution =
| Dynamic
| Static

let ( > ) r1 r2 =
    match (r1, r2) with
    | Dynamic, Static -> true
    | _ -> false

let ( < ) r1 r2 =
    match (r1, r2) with
    | Static, Dynamic -> true
    | _ -> false

let v_kind_to_string (vk : Wsize.v_kind) =
    match vk with
    | Const -> "Const"
    | Stack _ -> "Stack"
    | Reg _ -> "Reg"
    | Inline -> "Inline"
    | Global -> "Global"

let min_resolution (r1 : resolution) (r2 : resolution) : resolution =
    if r1 < r2 then
      r1
    else
      r2

let sup_resolution (r : resolution list) =
    List.fold_left
      (fun res r ->
        if r > res then
          r
        else
          res )
      Static r

let inf_resolution (r : resolution list) =
    List.fold_left
      (fun res r ->
        if r < res then
          r
        else
          res )
      Dynamic r

let var_resolution (v : 'len gvar) : resolution =
    match v.v_kind with
    | Inline -> Static
    | _ -> Dynamic

let var_of_lv (lv : 'len glval) : 'len gvar_i =
    match lv with
    | Lnone _ -> assert false
    | Lvar v -> v
    | Lmem (_, _, v, _)
     |Laset (_, _, _, v, _)
     |Lasub (_, _, _, v, _) ->
        v

let gvar_resolution (gv : 'len gvar_i) : resolution = var_resolution (L.unloc gv)
(*
  Compute the resolution time of an expression : if it contains a dynamic variable, then it must be dynamic
  It also check coherence between array and content expression: if the array is static, then the expression accessing the array must be static
  *)

let lval_static_vars (lvs : int glvals) : 'len gvar_i list =
    Sgv.to_list
      (List.fold
         (fun s (lv : int glval) ->
           match lv with
           | Lnone _ -> s
           | Lvar v
            |Lmem (_, _, v, _)
            |Laset (_, _, _, v, _)
            |Lasub (_, _, _, v, _) ->
               if gvar_resolution v = Static then
                 Sgv.add v s
               else
                 s )
         Sgv.empty lvs )

let expr_dynamic_vars expr : 'len gvar_i list =
    let rec sub_dynamics expr =
        match expr with
        | Pconst _ -> Sgv.empty
        | Pbool _ -> Sgv.empty
        | Parr_init _ -> Sgv.empty
        | Pvar v ->
            if gvar_resolution v.gv = Dynamic then
              Sgv.singleton v.gv
            else
              Sgv.empty
        | Pget (_, _, _, {gv= v; _}, e)
         |Psub (_, _, _, {gv= v; _}, e)
         |Pload (_, _, v, e) ->
            let s = sub_dynamics e in
            if gvar_resolution v = Dynamic then
              Sgv.add v s
            else
              s
        | Papp1 (_, e) -> sub_dynamics e
        | Papp2 (_, e1, e2) -> Sgv.union (sub_dynamics e1) (sub_dynamics e2)
        | PappN (_, es) -> List.fold (fun s e -> Sgv.union s (sub_dynamics e)) Sgv.empty es
        | Pif (_, e1, e2, e3) ->
            Sgv.union (sub_dynamics e1) (Sgv.union (sub_dynamics e2) (sub_dynamics e3))
    in
    Sgv.to_list (sub_dynamics expr)

let rec expr_resolution (expr : expr) : resolution =
    match expr with
    | Pconst _ -> Static
    | Pbool _ -> Static
    | Parr_init _ -> Static
    | Pvar v -> gvar_resolution v.gv
    | Pget (_, _, _, {gv= v; _}, e)
     |Psub (_, _, _, {gv= v; _}, e)
     |Pload (_, _, v, e) ->
        array_resolution_coherence v e
    | Papp1 (_, e) -> expr_resolution e
    | Papp2 (_, e1, e2) ->
        let e1_res = expr_resolution e1 in
        let e2_res = expr_resolution e2 in
        sup_resolution [e1_res; e2_res]
    | PappN (_, es) -> sup_resolution (List.map expr_resolution es)
    | Pif (_, e1, e2, e3) ->
        sup_resolution [expr_resolution e1; expr_resolution e2; expr_resolution e3]

and array_resolution_coherence var expr =
    let var_res = gvar_resolution var in
    let expr_res = expr_resolution expr in
    if var_res < expr_res then
      rs_iverror ~loc:(L.loc var) (AccessToStaticWithDynamic (L.unloc var, expr_dynamic_vars expr))
    else
      var_res

let lv_resolution (lv : 'len glval) : resolution =
    match lv with
    | Lnone _ -> Static
    | Lvar v -> gvar_resolution v
    | Lmem (_, _, v, e)
     |Laset (_, _, _, v, e)
     |Lasub (_, _, _, v, e) ->
        array_resolution_coherence v e

module FunctionResolution = struct
  type func_sig =
  { return_signature: resolution list
  ; args_signature: resolution list
  ; args: int gvar list
  ; resolution: resolution }

  type t = func_sig Mf.t

  let empty = Mf.empty

  let add (env : t) (f : ('info, 'asm) func) : t =
      let return_signature = List.map (fun v -> gvar_resolution v) f.f_ret in
      let args_signature = List.map (fun v -> var_resolution v) f.f_args in
      let args = f.f_args in
      let resolution =
          match f.f_cc with
          | Internal -> Static
          | _ -> Dynamic
      in
      Mf.add f.f_name {return_signature; args_signature; args; resolution} env

  let get (env : t) (f : funname) : func_sig = Mf.find f env

  let check_arguments loc (env : t) (f : funname) (args : 'len gexpr list) : unit =
      let f_sig = get env f in
      let def_sig = f_sig.args_signature in
      let arg_sig = List.map (fun e -> expr_resolution e) args in
      let zip = List.combine (List.combine def_sig arg_sig) (List.combine f_sig.args args) in
      List.iter
        (fun ((d, a), (var, expr)) ->
          if d < a then
            rs_iverror ~loc (StaticArgumentExpected (f, var, expr_dynamic_vars expr)) )
        zip

  let check_return loc (env : t) (f : funname) (lvs : 'len glval list) : unit =
      let ret_sig = (get env f).return_signature in
      let lv_sig = List.map (fun lv -> lv_resolution lv) lvs in
      let zip = List.combine (List.combine ret_sig lv_sig) lvs in
      List.iter
        (fun ((r, l), lv) ->
          if l < r then
            rs_iverror ~loc (DynamicReturntoStatic (f, var_of_lv lv)) )
        zip
end

let compute_function_resolution ((_, funcs) : ('info, 'asm) prog) : FunctionResolution.t =
    List.fold_left (fun env f -> FunctionResolution.add env f) FunctionResolution.empty funcs
