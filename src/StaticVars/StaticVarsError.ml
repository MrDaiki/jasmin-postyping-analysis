open Jasmin
open Prog

type 'len iv_error =
| StaticToDynamicAssign of 'len gvar_i * 'len gvar_i list
| AccessToStaticWithDynamic of var * 'len gvar_i list
| StaticArgumentExpected of funname * 'len gvar * 'len gvar_i list
| DynamicReturntoStatic of funname * 'len gvar_i
| SyscallToStatic of 'len gvar_i list * string
| IfConditionStatic of 'len gvar_i list * 'len gvar_i list
| ForVarStatic of 'len gvar_i * 'len gvar_i list
| ForStaticWithDynamicVariable of 'len gvar_i * 'len gvar_i list
| ForStaticWithDynamicRange of int gvar_i list * 'len gvar_i list
| WhileStaticWithDynamicCondition of int gvar_i list * 'len gvar_i list

let pp_iverror fmt error =
    match error with
    | StaticToDynamicAssign (v, dynvars) ->
        Format.fprintf fmt
          "Trying to assign inline variable %s with expression derived from dynamic variable : %a"
          (L.unloc v).v_name
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynvars)
    | AccessToStaticWithDynamic (v, vars) ->
        Format.fprintf fmt "Trying to access inline array %s with dynamic variables : %a" v.v_name
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc vars)
    | StaticArgumentExpected (f, v, vars) ->
        Format.fprintf fmt
          "In function `%s` call, argument `%s` is inline, but dynamic variables (%a) were passed"
          f.fn_name v.v_name
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc vars)
    | DynamicReturntoStatic (f, var) ->
        Format.fprintf fmt
          "Function `%s` return a dynamic value, but it is assigned to inline variable `%s`"
          f.fn_name (L.unloc var).v_name
    | SyscallToStatic (static_vars, syscall) ->
        Format.fprintf fmt
          "Syscall `#%s`return a dynamic value, but it is assigned to static variables : %a" syscall
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc static_vars)
    | IfConditionStatic (static_vars, dynamic_vars) ->
        Format.fprintf fmt
          "If bloc modify inline variables : %a, and thus it's condition must be known at compile \
           time.@.But it contains dynamic variables : %a"
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc static_vars)
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynamic_vars)
    | ForVarStatic (v, dynamic_vars) ->
        Format.fprintf fmt "For loop variable `%s` is inline, but it's range is dynamic : %a"
          (L.unloc v).v_name
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynamic_vars)
    | ForStaticWithDynamicRange (static_vars, dynamic_vars) ->
        Format.fprintf fmt
          "For loop bloc modify inline variables : %a, and thus it's range must be known at \
           compile time.@.But range contains dynamic variables : %a"
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc static_vars)
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynamic_vars)
    | ForStaticWithDynamicVariable (v, dynamic_vars) ->
        Format.fprintf fmt
          "For loop bloc modify inline variable `%a`, but loop variable %s is dynamic, and thus \
           can't be known at compile time"
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynamic_vars) (L.unloc v).v_name
    | WhileStaticWithDynamicCondition (static_vars, dynamic_vars) ->
        Format.fprintf fmt
          "While bloc modify inline variables : %a, and thus it's condition must be known at \
           compile time.@.But it contains dynamic variables : %a"
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc static_vars)
          (Utils.pp_list ", " (Printer.pp_var ~debug:false))
          (List.map L.unloc dynamic_vars)

exception IVError of L.t * int iv_error

let iverror ~loc (code : int iv_error) = IVError (loc, code)

let rs_iverror ~loc (code : int iv_error) = raise (iverror ~loc code)
