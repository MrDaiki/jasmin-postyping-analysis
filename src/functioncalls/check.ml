open Jasmin
open Prog
open Warning
open FInfo
open Visitor
open Programvisitor

module FunctionCallPartialVisitor :
  PartialVisitor with type data = Sf.t and type annotation = unit = struct
  type data = Sf.t

  type annotation = unit

  let initial_state : data = Sf.empty

  let visit_funcall (_ : L.i_loc) (_ : annotation) _ funname _ data : data = Sf.add funname data

  let visit_syscall (_ : L.i_loc) (_ : annotation) _ _ _ data : data = data

  let visit_assign (_ : L.i_loc) (_ : annotation) _ _ _ _ data : data = data

  let visit_copn (_ : L.i_loc) (_ : annotation) _ _ _ _ data : data = data

  let rec visit_for visit_instr (_ : L.i_loc) (_ : annotation) _ _ bloc data : data =
      visit_stmt visit_instr bloc data

  and visit_while visit_instr (_ : L.i_loc) (_ : annotation) _ _ b1 _ b2 data : data =
      let data = visit_stmt visit_instr b1 data in
      visit_stmt visit_instr b2 data

  and visit_if visit_instr (_ : L.i_loc) (_ : annotation) _ th el data : data =
      let data = visit_stmt visit_instr th data in
      visit_stmt visit_instr el data

  and visit_stmt visit_instr stmt data : data =
      List.fold_left (fun acc instr -> visit_instr instr acc) data stmt

  let visit_function visit_instr (func : (annotation, 'asm) func) data : data =
      visit_stmt visit_instr func.f_body data

  let visit_prog visit_instr ((_, funcs) : (annotation, 'asm) prog) data : data =
      List.fold_left (fun acc f -> visit_function visit_instr f acc) data funcs
end

module FunctionCallVisitor : Visitor.S with type data = Sf.t and type annotation = unit =
  Visitor.Make (FunctionCallPartialVisitor)

let non_export_functions (funcs : ('len, 'info, 'asm) gfunc list) =
    List.fold_left
      (fun acc f ->
        if not (is_export f.f_cc) then
          Sf.add f.f_name acc
        else
          acc )
      Sf.empty funcs

let fc_prog (prog : ('info, 'asm) prog) : unit =
    let _, funcs = prog in
    let data = FunctionCallVisitor.initial_state in
    let funcs_calls = FunctionCallVisitor.visit_prog prog data in
    let non_exported = non_export_functions funcs in
    let diff = Sf.diff non_exported funcs_calls in
    Sf.iter (fun f -> pp_unf_warning Format.std_formatter (UnusedFunction f)) diff
