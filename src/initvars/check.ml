open Jasmin
open Prog
open Visitor
open Programvisitor
open Helper
open Rd.Rdanalyser

module InitVarPartialVisitor :
  PartialVisitor with type data = iv_data and type annotation = Rd.Domain.Domain.t = struct
  type data = iv_data

  type annotation = Rd.Domain.Domain.t

  let initial_state : data = {locals= Sv.empty; mode= Strict; errors= []}

  let visit_funcall
      (_ : L.i_loc)
      (domain : annotation)
      (_ : int glvals)
      (_ : funname)
      (params : int gexprs)
      (data : data) : data =
      List.fold_left (fun d e -> check_iv_expr d domain e) data params

  let visit_syscall
      (_ : L.i_loc)
      (domain : annotation)
      (_ : int glvals)
      (_ : 'asm Syscall_t.syscall_t)
      (params : int gexprs)
      (data : data) : data =
      List.fold_left (fun d e -> check_iv_expr d domain e) data params

  let visit_assign
      (_ : L.i_loc)
      (domain : annotation)
      (_ : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (expr : int gexpr)
      (data : data) : data =
      check_iv_expr data domain expr

  let visit_copn
      (_ : L.i_loc)
      (domain : annotation)
      (_ : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (exprs : int gexprs)
      (data : data) : data =
      List.fold_left (fun d e -> check_iv_expr d domain e) data exprs

  let rec visit_for
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (domain : annotation)
      (_ : int gvar_i)
      ((_, r1, r2) : int grange)
      (stmt : (annotation, 'asm) stmt)
      (data : data) : data =
      let data = check_iv_expr data domain r1 in
      let data = check_iv_expr data domain r2 in
      visit_stmt visit_instr stmt data

  and visit_while
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (_ : annotation)
      ((_, domain) : IInfo.t * annotation)
      (_ : E.align)
      (b1 : (annotation, 'asm) stmt)
      (cond : int gexpr)
      (b2 : (annotation, 'asm) stmt)
      (data : data) : data =
      let data = visit_stmt visit_instr b1 data in
      let data = check_iv_expr data domain cond in
      visit_stmt visit_instr b2 data

  and visit_if
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (_ : L.i_loc)
      (domain : annotation)
      (cond : int gexpr)
      (th : (annotation, 'asm) stmt)
      (el : (annotation, 'asm) stmt)
      (data : data) : data =
      let data = check_iv_expr data domain cond in
      let d1 = visit_stmt visit_instr th data in
      visit_stmt visit_instr el d1

  and visit_stmt visit_instr stmt data : data =
      List.fold_left (fun d i -> visit_instr i d) data stmt

  let visit_function
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (func : (annotation, 'asm) func)
      data : data =
      let data = {data with locals= Prog.locals func} in
      let data = visit_stmt visit_instr func.f_body data in
      List.fold_left (fun d -> check_iv_error d func.f_info) data func.f_ret

  let visit_prog
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      ((_, funcs) : (annotation, 'asm) prog)
      data : data =
      List.fold_left (fun d f -> visit_function visit_instr f d) data funcs
end

module InitVarVisitor :
  Visitor.S
    with type annotation = InitVarPartialVisitor.annotation
     and type data = InitVarPartialVisitor.data =
  Visitor.Make (InitVarPartialVisitor)

let iv_prog ((globs, funcs) : ('info, 'asm) prog) (strict : bool) =
    let strict =
        if strict then
          Strict
        else
          NotStrict
    in
    let funcs =
        List.map
          (fun f ->
            let f, _ =
                ReachingDefinitionAnalyser.analyse_function f
                  (Rd.Domain.Domain.from_function_start f)
            in
            f )
          funcs
    in
    let prog = (globs, funcs) in
    let data = InitVarVisitor.initial_state in
    let data = {data with mode= strict} in
    let data = InitVarVisitor.visit_prog prog data in
    List.rev data.errors
