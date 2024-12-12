open Jasmin
open Prog
open Syscall_t
open Visitor.Programvisitor
open Resolution
open Error

let name_of_syscall sc =
    match sc with
    | RandomBytes _ -> "randombytes"

let build_visitor (prog : ('info, 'asm) prog) =
    let fres = compute_function_resolution prog in
    let module StaticVarsPartialVisitor :
      PartialVisitor with type data = resolution * int gvar_i list and type annotation = unit =
    struct
      type data = resolution * int gvar_i list

      type annotation = unit

      let initial_state : data = (Dynamic, [])

      let visit_funcall
          (loc : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (funname : funname)
          (params : int gexprs)
          ((res, statics) : data) : data =
          FunctionResolution.check_arguments loc.base_loc fres funname params ;
          FunctionResolution.check_return loc.base_loc fres funname lvs ;
          let lvs_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
          (min_resolution res lvs_res, lval_static_vars lvs @ statics)

      let visit_syscall
          (loc : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (syscall : 'asm Syscall_t.syscall_t)
          (_ : int gexprs)
          (data : data) : data =
          let lv_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
          if lv_res = Static then
            let sc_name = name_of_syscall syscall in
            rs_iverror ~loc:loc.base_loc (SyscallToStatic (lval_static_vars lvs, sc_name))
          else
            data

      let _assign_logic
          (lv : int glval)
          (expr : int gexpr)
          (loc : L.i_loc)
          ((resolution, statics) : data) : data =
          let lv_res = lv_resolution lv in
          let expr_res = expr_resolution expr in
          if expr_res > lv_res then
            rs_iverror ~loc:loc.base_loc
              (StaticToDynamicAssign (var_of_lv lv, expr_dynamic_vars expr))
          else
            let statics =
                match lv_res with
                | Dynamic -> statics
                | Static -> var_of_lv lv :: statics
            in
            (min_resolution expr_res resolution, statics)

      let visit_assign
          (loc : L.i_loc)
          (_ : annotation)
          (lv : int glval)
          (_ : E.assgn_tag)
          (_ : int gty)
          (expr : int gexpr)
          (data : data) : data =
          _assign_logic lv expr loc data

      let visit_copn
          (loc : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (_ : E.assgn_tag)
          (_ : 'asm Sopn.sopn)
          (exprs : int gexprs)
          (data : data) : data =
          let zip = List.combine lvs exprs in
          List.fold_left (fun (s : data) (lv, expr) -> _assign_logic lv expr loc s) data zip

      let rec visit_for
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          (loc : L.i_loc)
          (_ : annotation)
          (var : int gvar_i)
          ((_, s, e) : int grange)
          (stmt : (annotation, 'asm) stmt)
          ((res, statics) : data) : data =
          let res, statics = visit_stmt visit_instr stmt (res, statics) in
          let range_res = min_resolution (expr_resolution s) (expr_resolution e) in
          if range_res < res then
            rs_iverror ~loc:loc.base_loc
              (ForStaticWithDynamicRange (statics, expr_dynamic_vars s @ expr_dynamic_vars e))
          else if gvar_resolution var < res then
            rs_iverror ~loc:loc.base_loc
              (ForStaticWithDynamicVariable (var, expr_dynamic_vars s @ expr_dynamic_vars e))
          else
            let statics =
                match gvar_resolution var with
                | Dynamic -> statics
                | Static -> var :: statics
                (*We add it here because it could be conflicting if added while checking declaration*)
            in
            (res, statics)

      and visit_while
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          (loc : L.i_loc)
          (_ : annotation)
          (_ : IInfo.t * annotation)
          (_ : E.align)
          (b1 : (annotation, 'asm) stmt)
          (cond : int gexpr)
          (b2 : (annotation, 'asm) stmt)
          (data : data) : data =
          let while_res, while_statics = visit_stmt visit_instr b1 data in
          let while_res, while_statics = visit_stmt visit_instr b2 (while_res, while_statics) in
          let condition_res = expr_resolution cond in
          if condition_res < while_res then
            rs_iverror ~loc:loc.base_loc
              (WhileStaticWithDynamicCondition (while_statics, expr_dynamic_vars cond))
          else
            (while_res, while_statics)

      and visit_if
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          (loc : L.i_loc)
          (_ : annotation)
          (cond : int gexpr)
          (th : (annotation, 'asm) stmt)
          (el : (annotation, 'asm) stmt)
          (data : data) : data =
          let data = visit_stmt visit_instr th data in
          let res, statics = visit_stmt visit_instr el data in
          let condition_res = expr_resolution cond in
          if condition_res < res then
            rs_iverror ~loc:loc.base_loc (IfConditionStatic (statics, expr_dynamic_vars cond))
          else
            (res, statics)

      and visit_stmt visit_instr stmt data : data =
          List.fold_left (fun data instr -> visit_instr instr data) data stmt

      let visit_function
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          (func : (annotation, 'asm) func)
          data : data =
          visit_stmt visit_instr func.f_body data

      let visit_prog
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          ((_, funcs) : (annotation, 'asm) prog)
          data : data =
          let _ = List.map (fun func -> visit_function visit_instr func data) funcs in
          (Dynamic, [])
    end in
    let module V = Visitor.Make (StaticVarsPartialVisitor) in
    (module V : Visitor.S with type data = resolution * int gvar_i list and type annotation = unit)

let iv_prog prog =
    let (module V) = build_visitor prog in
    V.visit_prog prog V.initial_state
