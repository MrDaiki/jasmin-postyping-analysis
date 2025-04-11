open Jasmin.Prog
open Visitor.ProgramVisitor
open DeadCodeError
open Liveness.LivenessAnalyser
open Jasmin

type self = {errors: Error.CompileError.compile_error list}

module PartialDeadCodeVisitor : PartialVisitor with type data = self and type annotation = Sv.t =
struct
  type data = self

  type annotation = Sv.t

  let visit_copn
      (loc : L.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : exprs)
      (data : data) : data =
      let lvs_vars = List.fold_left (fun s lv -> Jasmin.Prog.vars_lv s lv) Sv.empty lvs in
      let errors =
          Sv.fold
            (fun var acc ->
              match Sv.find_opt var annot with
              | Some _ -> acc
              | _ ->
                  let err = create_dead_code_error var loc.base_loc in
                  err :: acc )
            lvs_vars []
      in
      {errors= errors @ data.errors}

  let visit_assign
      (loc : L.i_loc)
      (annot : annotation)
      (lv : lval)
      (_ : E.assgn_tag)
      (_ : ty)
      (_ : expr)
      (data : data) : data =
      let lvs_vars = Jasmin.Prog.vars_lv Sv.empty lv in
      let errors =
          Sv.fold
            (fun var acc ->
              match Sv.find_opt var annot with
              | Some _ -> acc
              | _ ->
                  let err = create_dead_code_error var loc.base_loc in
                  err :: acc )
            lvs_vars []
      in
      {errors= errors @ data.errors}

  let visit_syscall
      (loc : Location.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (_ : exprs)
      (data : data) : data =
      let lvs_vars = List.fold_left (fun s lv -> Jasmin.Prog.vars_lv s lv) Sv.empty lvs in
      let errors =
          Sv.fold
            (fun var acc ->
              match Sv.find_opt var annot with
              | Some _ -> acc
              | _ ->
                  let err = create_dead_code_error var loc.base_loc in
                  err :: acc )
            lvs_vars []
      in
      {errors= errors @ data.errors}

  let visit_funcall
      (loc : Location.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (_ : funname)
      (_ : exprs)
      (data : data) : data =
      let lvs_vars = List.fold_left (fun s lv -> Jasmin.Prog.vars_lv s lv) Sv.empty lvs in
      let errors =
          Sv.fold
            (fun var acc ->
              match Sv.find_opt var annot with
              | Some _ -> acc
              | _ ->
                  let err = create_dead_code_error var loc.base_loc in
                  err :: acc )
            lvs_vars []
      in
      {errors= errors @ data.errors}

  let rec visit_if
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : expr)
      (th : (annotation, 'asm) Prog.stmt)
      (el : (annotation, 'asm) Prog.stmt)
      (data : data) : data =
      let data_th = visit_stmt visit_instr th data in
      let data_el = visit_stmt visit_instr el data in
      {errors= data_th.errors @ data_el.errors}

  and visit_while
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : IInfo.t * annotation)
      (_ : E.align)
      (b1 : (annotation, 'asm) Prog.stmt)
      (_ : expr)
      (b2 : (annotation, 'asm) Prog.stmt)
      (data : data) : data =
      let data = visit_stmt visit_instr b1 data in
      let data = visit_stmt visit_instr b2 data in
      data

  and visit_for
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      (_ : var_i)
      (_ : int grange)
      (stmt : (annotation, 'asm) Prog.stmt)
      (data : data) : data =
      visit_stmt visit_instr stmt data

  and visit_stmt
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      (stmt : (annotation, 'asm) Prog.stmt)
      (data : data) : data =
      List.fold_left (fun acc i -> visit_instr i acc) data stmt

  let visit_function
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      (func : (annotation, 'asm) Prog.func)
      (data : data) : data =
      visit_stmt visit_instr func.f_body data

  let visit_prog
      (visit_instr : (annotation, 'asm) Prog.instr -> data -> data)
      ((_, funcs) : (annotation, 'asm) Prog.prog)
      (data : data) : data =
      List.fold_left (fun acc f -> visit_function visit_instr f acc) data funcs
end

module DeadCodeVisitor : Visitor.S with type data = self and type annotation = Sv.t =
  Visitor.Make (PartialDeadCodeVisitor)

let initial_state : self = {errors= []}

let dc_prog ((globs, funcs) : ('info, 'asm) prog) =
    let funcs =
        List.map
          (fun f ->
            let f = LivenessAnalyser.analyse_function f in
            f )
          funcs
    in
    let prog = (globs, funcs) in
    let data = DeadCodeVisitor.visit_prog prog initial_state in
    (prog, List.rev data.errors)
