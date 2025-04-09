open Visitor.ProgramVisitor
open Jasmin.Prog
open Jasmin.Wsize

type leakage_data = {function_sig: ty list Mf.t}

type ptr_kind =
| Stack
| Reg

let expr_stack_or_ptr (expr : expr) =
    match expr with
    | Pconst _
     |Pbool _
     |Parr_init _
     |Papp1 _
     |Papp2 _
     |PappN _
     |Pif _ ->
        None
    | Pvar v -> (
        match (L.unloc v.gv).v_kind with
        | Const
         |Inline
         |Global ->
            None
        | Stack (Pointer _) -> Some Stack (* stack pointer*)
        | Reg (_, Pointer _) -> Some Reg (* reg pointer*)
        | Stack _ -> None
        | Reg _ -> None )
    | Pget _
     |Psub _
     |Pload _ ->
        assert false (* this is not possible in my knowledge in Jasmin*)

module PartialLeakageVisitor :
  PartialVisitor with type data = leakage_data and type annotation = unit = struct
  type data = leakage_data

  type annotation = unit

  let visit_funcall
      (loc : L.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (funname : funname)
      (params : exprs)
      (data : data) : data =
      data

  let visit_syscall
      (loc : L.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (syscall : Jasmin.BinNums.positive Jasmin.Syscall_t.syscall_t)
      (params : exprs)
      (data : data) : data =
      data

  let visit_assign
      (loc : L.i_loc)
      (annot : annotation)
      (lv : lval)
      (tag : E.assgn_tag)
      (gty : ty)
      (expr : expr)
      (data : data) : data =
      data

  let visit_copn
      (loc : L.i_loc)
      (annot : annotation)
      (lvs : lvals)
      (tag : E.assgn_tag)
      (opn : 'asm Jasmin.Sopn.sopn)
      (exprs : exprs)
      (data : data) : data =
      data

  let visit_for
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (loc : L.i_loc)
      (annot : annotation)
      (var : var_i)
      (range : int grange)
      (stmt : (annotation, 'asm) stmt)
      (data : data) : data =
      data

  let visit_while
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (loc : L.i_loc)
      (annot : annotation)
      (info : Jasmin.IInfo.t * annotation)
      (align : E.align)
      (b1 : (annotation, 'asm) stmt)
      (cond : expr)
      (b2 : (annotation, 'asm) stmt)
      (data : data) : data =
      data

  let visit_if
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (loc : L.i_loc)
      (annot : annotation)
      (cond : expr)
      (th : (annotation, 'asm) stmt)
      (el : (annotation, 'asm) stmt)
      (data : data) : data =
      data

  let visit_stmt
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (stmt : (annotation, 'asm) stmt)
      (data : data) : data =
      data

  let visit_function
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (f : (annotation, 'asm) func)
      (data : data) : data =
      data

  let visit_prog
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (prog : (annotation, 'asm) prog)
      (data : data) : data =
      data
end

let empty = {function_sig= Mf.empty}

let initial_state (_, funcs) =
    let data = empty in
    let signatures = List.fold_left (fun acc f -> Mf.add f.f_name f.f_ty acc) Mf.empty funcs in
    {data with function_sig= signatures}
