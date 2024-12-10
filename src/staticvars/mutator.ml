(* let name_of_syscall sc =
    match sc with
    | RandomBytes _ -> "randombytes" *)

(* module Ivmutator :
     SimpleMutator with type state = resolution * int gvar_i list * FunctionResolution.t = struct
     type state = resolution * int gvar_i list * FunctionResolution.t

     type annotation = unit

     let loop_stop_condition _ _ = true

     let cif (condition : int gexpr) (loc : L.i_loc) (th_res, th_statics, fres) (el_res, el_statics, _)
         : state * annotation =
         let res = min_resolution th_res el_res in
         let statics = th_statics @ el_statics in
         let condition_res = expr_resolution condition in
         if condition_res < res then
           rs_iverror ~loc:loc.base_loc (IfConditionStatic (statics, expr_dynamic_vars condition))
         else
           ((res, statics, fres), ())

     let cfor
         (v : 'len gvar_i)
         ((_, s, e) : 'len grange)
         (loc : L.i_loc)
         (_ : state)
         ((for_res, for_statics, fres) : state) : state * annotation =
         let sres, eres = (expr_resolution s, expr_resolution e) in
         let range_res = min_resolution sres eres in
         if range_res < for_res then
           rs_iverror ~loc:loc.base_loc
             (ForStaticWithDynamicRange (for_statics, expr_dynamic_vars s @ expr_dynamic_vars e))
         else if gvar_resolution v < for_res then
           rs_iverror ~loc:loc.base_loc
             (ForStaticWithDynamicVariable (v, expr_dynamic_vars s @ expr_dynamic_vars e))
         else
           let for_statics =
               match gvar_resolution v with
               | Dynamic -> for_statics
               | Static -> v :: for_statics
               (*We add it here because it could be conflicting if added while checking declaration*)
           in
           ((for_res, for_statics, fres), ())

     let cfor_decl
         (v : int gvar_i)
         ((_, s, e) : 'len grange)
         (loc : L.i_loc)
         ((for_res, for_statics, fres) : state) : state =
         let sres, eres = (expr_resolution s, expr_resolution e) in
         let range_res = min_resolution sres eres in
         if range_res > gvar_resolution v then
           rs_iverror ~loc:loc.base_loc (ForVarStatic (v, expr_dynamic_vars s @ expr_dynamic_vars e))
         else
           (min_resolution for_res range_res, for_statics, fres)

     let cwhile
         (_ : E.align)
         (condition : 'len gexpr)
         (loc : L.i_loc)
         (_ : state)
         ((while_res, while_statics, fres) : state) : state * annotation =
         let condition_res = expr_resolution condition in
         if condition_res < while_res then
           rs_iverror ~loc:loc.base_loc
             (WhileStaticWithDynamicCondition (while_statics, expr_dynamic_vars condition))
         else
           ((while_res, while_statics, fres), ())

     let assign_logic
         (lv : int glval)
         (expr : 'len gexpr)
         (loc : L.i_loc)
         ((resolution, statics, fres) : state) : state * annotation =
         let lv_res = lv_resolution lv in
         let expr_res = expr_resolution expr in
         if expr_res > lv_res then
           rs_iverror ~loc:loc.base_loc (StaticToDynamicAssign (var_of_lv lv, expr_dynamic_vars expr))
         else
           let statics =
               match lv_res with
               | Dynamic -> statics
               | Static -> var_of_lv lv :: statics
           in
           ((min_resolution expr_res resolution, statics, fres), ())

     let cassign
         (lv : int glval)
         (_ : E.assgn_tag)
         (_ : 'len gty)
         (expr : 'len gexpr)
         (loc : L.i_loc)
         (state : state) : state * annotation =
         assign_logic lv expr loc state

     let fcall
         (lvs : int glvals)
         (fn : funname)
         (exprs : int gexprs)
         (loc : L.i_loc)
         ((res, statics, fres) : state) : state * annotation =
         FunctionResolution.check_arguments loc.base_loc fres fn exprs ;
         FunctionResolution.check_return loc.base_loc fres fn lvs ;
         let lvs_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
         ((min_resolution res lvs_res, lval_static_vars lvs @ statics, fres), ())

     let syscall
         (lvs : int glvals)
         (sc : BinNums.positive Syscall_t.syscall_t)
         (_ : 'len gexprs)
         (loc : L.i_loc)
         (state : state) : state * annotation =
         let lv_res = inf_resolution (List.map (fun lv -> lv_resolution lv) lvs) in
         if lv_res = Static then
           let sc_name = name_of_syscall sc in
           rs_iverror ~loc:loc.base_loc (SyscallToStatic (lval_static_vars lvs, sc_name))
         else
           (state, ())

     let copn
         (lvs : int glvals)
         (_ : E.assgn_tag)
         (_ : 'asm Sopn.sopn)
         (exprs : int gexprs)
         (loc : L.i_loc)
         (state : state) : state * annotation =
         let zip = List.combine lvs exprs in
         List.fold_left
           (fun ((s, _) : state * annotation) (lv, expr) -> assign_logic lv expr loc s)
           (state, ()) zip
   end

   module IvWalker = TreeWalker (Ivmutator)

   let iv_prog ((g, funcs) : ('info, 'asm) prog) =
       let fres = compute_function_resolution (g, funcs) in
       let state = (Dynamic, ([] : int gvar_i list), fres) in
       List.iter (fun f -> ignore (IvWalker.walk_func f state)) funcs *)
