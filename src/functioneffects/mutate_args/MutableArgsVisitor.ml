open Jasmin
open Prog
open Utils
open Mutparameffect
open Visitor.Programvisitor

type visitor_data =
{ mutable_params_set: Sv.t
; function_params_map: int gvar list Mf.t
; mutable_params_effect: mut_param_effect Mv.t }

let is_mutable_ptr var =
    match var.v_kind with
    | Reg (_, Pointer _)
     |Stack (Pointer _) ->
        true
    | _ -> false

let effect_of_lv (lv : int glval) (mut_params : Sv.t) : mut_param_effect =
    match lv with
    | Lmem (_, _, var, _)
     |Laset (_, _, _, var, _)
     |Lasub (_, _, _, var, _)
     |Lvar var ->
        if Sv.mem (L.unloc var) mut_params then
          Some
        else
          None
    | _ -> None

let variable_of_arg (expr : int gexpr) (mut_params : Sv.t) : var option =
    match expr with
    | Pvar v ->
        if Sv.mem (L.unloc v.gv) mut_params then
          Some (L.unloc v.gv)
        else
          None
    | _ -> None

let check_dependency_on_argument
    (funname : funname)
    (index : int)
    (expr : int gexpr)
    (data : visitor_data) : visitor_data =
    let expr_var = variable_of_arg expr data.mutable_params_set in
    match expr_var with
    | None -> data
    | Some var ->
        let function_args = Mf.find funname data.function_params_map in
        let arg = List.nth function_args index in
        { data with
          mutable_params_effect=
            Mv.modify_def None var
              (fun s -> s || Depends (Sv.singleton arg))
              data.mutable_params_effect }

let update_effect_lv (lv : int glval) (data : visitor_data) : visitor_data =
    match lv with
    | Lvar var
     |Lmem (_, _, var, _)
     |Laset (_, _, _, var, _)
     |Lasub (_, _, _, var, _) ->
        { data with
          mutable_params_effect=
            Mv.modify_def None (L.unloc var)
              (fun s -> s || effect_of_lv lv data.mutable_params_set)
              data.mutable_params_effect }
    | Lnone _ -> data

let build_mutable_params_set funcs =
    List.fold
      (fun acc func ->
        let mutparams =
            List.fold
              (fun acc' var ->
                if is_mutable_ptr var then
                  Sv.add var acc'
                else
                  acc' )
              Sv.empty func.f_args
        in
        Sv.union mutparams acc )
      Sv.empty funcs

let function_params_map funcs =
    List.fold (fun acc func -> Mf.add func.f_name func.f_args acc) Mf.empty funcs

module MutateArgsPartialVisitor :
  PartialVisitor with type data = visitor_data and type annotation = unit = struct
  type data = visitor_data

  type annotation = unit

  let initial_state : data =
      {mutable_params_set= Sv.empty; function_params_map= Mf.empty; mutable_params_effect= Mv.empty}

  let visit_funcall
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (funname : funname)
      (params : int gexprs)
      (data : data) : data =
      List.fold_lefti
        (fun data i (lv, expr) ->
          let data = check_dependency_on_argument funname i expr data in
          update_effect_lv lv data )
        data (List.combine lvs params)

  let visit_syscall
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (_ : 'asm Syscall_t.syscall_t)
      (_ : int gexprs)
      (data : data) : data =
      List.fold (fun acc lv -> update_effect_lv lv acc) data lvs

  let visit_assign
      (_ : L.i_loc)
      (_ : annotation)
      (lv : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (_ : int gexpr)
      (data : data) : data =
      update_effect_lv lv data

  let visit_copn
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (data : data) : data =
      List.fold (fun acc lv -> update_effect_lv lv acc) data lvs

  let rec visit_for
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : int gvar_i)
      (_ : int grange)
      (stmt : (annotation, 'asm) stmt)
      (data : data) : data =
      visit_stmt visit_instr stmt data

  and visit_while
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : IInfo.t * annotation)
      (_ : E.align)
      (b1 : (annotation, 'asm) stmt)
      (_ : int gexpr)
      (b2 : (annotation, 'asm) stmt)
      (data : data) : data =
      let data = visit_stmt visit_instr b1 data in
      visit_stmt visit_instr b2 data

  and visit_if
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : int gexpr)
      (th : (annotation, 'asm) stmt)
      (el : (annotation, 'asm) stmt)
      (data : data) : data =
      let data = visit_stmt visit_instr th data in
      visit_stmt visit_instr el data

  and visit_stmt visit_instr stmt data : data =
      List.fold_left (fun d i -> visit_instr i d) data stmt

  let visit_function
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (func : (annotation, 'asm) func)
      data : data =
      let intial_state =
          List.fold
            (fun acc' var ->
              if is_mutable_ptr var then
                Mv.add var None acc'
              else
                acc' )
            Mv.empty func.f_args
      in
      let data = {data with mutable_params_effect= intial_state} in
      visit_stmt visit_instr func.f_body data

  let visit_prog
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      ((_, funcs) : (annotation, 'asm) prog)
      data : data =
      let data =
          { data with
            mutable_params_set= build_mutable_params_set funcs
          ; function_params_map= function_params_map funcs }
      in
      List.fold_left
        (fun d f ->
          let data = visit_function visit_instr f d in
          { d with
            mutable_params_effect=
              Mv.merge
                (fun _ a b ->
                  match (a, b) with
                  | None, None -> None
                  | Some x, None
                   |None, Some x ->
                      Some x
                  | _ -> assert false )
                d.mutable_params_effect data.mutable_params_effect } )
        data funcs
end

module MutableArgsVisitor :
  Visitor.S
    with type data = MutateArgsPartialVisitor.data
     and type annotation = MutateArgsPartialVisitor.annotation =
  Visitor.Make (MutateArgsPartialVisitor)
