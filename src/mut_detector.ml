open Jasmin

open Prog 
open Utils
open CoreIdent
open Annotations

type md_error = 
| AssignOnNonMutablePtr of (symbol)
| FunctionArgNotMutable of funname * symbol * symbol


let pp_ptyerror fmt (error:md_error) =
  match error with 
  | AssignOnNonMutablePtr id -> 
    Format.fprintf fmt "Trying to mutate %s, which is a non mutable pointer" id
  | FunctionArgNotMutable (fn,id_arg,id_var) -> 
    Format.fprintf fmt "in function %s call, argument %s is expected to be mutable, but argument %s is not mutable" fn.fn_name id_arg id_var

exception MdError of L.t * md_error

let p_mderror ~loc (code:md_error) = 
  MdError (loc,code)
let rs_mderror ~loc (code:md_error) = 
  raise (p_mderror ~loc code)



type mutability = Constant | Mutable | NotPtr

module Variables = struct 

  type t = {
    mutable_vars : Sv.t;
    functions : (mutability * symbol) list Mf.t;
  }

  let empty = {
    mutable_vars = Sv.empty;
    functions = Mf.empty;
  }
  let add_mutable (vars : t) (v: var) : t = 
    { vars with mutable_vars = Sv.add v vars.mutable_vars }
  

  let add_function_arguments (vars : t) (f: funname) (args: 'len gvar list ) : t =
    let args_mut = List.map (fun gv ->
      match gv.v_kind with
      | Reg (_, wr) | Stack wr ->
        begin
        match wr with 
        | Direct -> NotPtr,gv.v_name
        | Pointer wt ->
          match wt with
          | Constant -> Constant,gv.v_name
          | Writable -> Mutable,gv.v_name
        end
      | _ -> NotPtr,gv.v_name
    ) args in 
    { vars with functions = Mf.add f args_mut vars.functions }
  
  let check_mutable (vars : t) (v: var) : bool =
    Sv.mem v vars.mutable_vars

end

let add_variable_to_env (variables:Variables.t) (v: int gvar) : Variables.t =
  match v.v_kind with 
  | Reg (_, wr) | Stack wr -> 
    begin
    match wr with 
    | Direct -> variables
    | Pointer wt -> 
      match wt with
      | Constant -> variables
      | Writable -> Variables.add_mutable variables v
    end
  | _ -> variables


let md_lvalue (variables:Variables.t) (lv: 'len glval) : unit =
  match lv with 
  | Lnone _ -> ()
  | Lmem  (_,_,gv,_) | Laset (_,_,_,gv,_)| Lasub (_,_,_,gv,_) | Lvar gv -> 
    if (not (Variables.check_mutable variables (L.unloc gv))) 
    then
      rs_mderror ~loc:(L.loc gv) (AssignOnNonMutablePtr (L.unloc gv).v_name)

type x = int ggvar
let md_func_args (variables:Variables.t) (fun_name: funname) (args: 'len gexpr list) : unit =
  let rec cmp_mutability (fsig: (mutability * symbol) list) (args: expr list) : unit = 
    match fsig, args with
    | (NotPtr,_)::s, _::a -> cmp_mutability s a
    | (Mutable,param)::s, arg::a -> 
      begin 
        match arg with 
        | Pvar gv -> 
          if (not (Variables.check_mutable variables (L.unloc gv.gv))) then
            rs_mderror ~loc:(L.loc gv.gv) (FunctionArgNotMutable (fun_name, param, (L.unloc gv.gv).v_name))
          else
            cmp_mutability s a
        | _ -> assert false
      end
    | (Constant,_)::s, _::a -> cmp_mutability s a
    | [], [] -> ()
    | _,[] -> assert false
    | [], _ -> assert false
    in 
    cmp_mutability (Mf.find fun_name variables.functions) args
  

let md_lvalues (variables:Variables.t) (lv: 'len glval list) : unit =
  List.iter (md_lvalue variables) lv

let md_instr (variables:Variables.t) (i: ('info,'asm) instr) : unit =
  match i.i_desc with 
  | Cassgn (lv,_,_,_) -> 
    md_lvalues variables [lv]
  | Copn(lvs ,_ , _ ,_) -> 
    md_lvalues variables lvs
  | Csyscall (glvs,_,_) -> 
    md_lvalues variables glvs
  | Ccall (lvs,fun_name,exprs) -> 
    md_lvalues variables lvs;
    md_func_args variables fun_name exprs
  | _ -> ()


let md_stmt (variables:Variables.t) (s: ('info,'asm) stmt) : unit =
  List.iter (md_instr variables) s



let md_params (variables:Variables.t) (var:'len gvar) : Variables.t =
  add_variable_to_env variables var

let md_func (variables:Variables.t) (f: ('info,'asm) func) : unit =
  let variables = List.fold (fun v g -> md_params v g) variables f.f_args in
  md_stmt variables f.f_body


let md_glob (variables:Variables.t) (v,_: (int gvar * Global.glob_value)) : Variables.t =
  add_variable_to_env variables v

let md_prog (variables:Variables.t) ((globs,funcs): ('info,'asm) prog)  : unit =
  let variables = List.fold (fun v g -> md_glob v g) variables globs in
  List.iter (md_func variables) funcs