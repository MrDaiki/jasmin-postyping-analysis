open Jasmin
open Prog

module type PartialVisitor = sig
  type data

  type annotation

  val initial_state : data

  val visit_prog :
    ((annotation, 'asm) Prog.instr -> data -> data) -> (annotation, 'asm) Prog.prog -> data -> data

  val visit_function :
    ((annotation, 'asm) Prog.instr -> data -> data) -> (annotation, 'asm) Prog.func -> data -> data

  val visit_stmt :
    ((annotation, 'asm) Prog.instr -> data -> data) -> (annotation, 'asm) Prog.stmt -> data -> data

  val visit_funcall : int glvals -> funname -> int gexprs -> data -> data

  val visit_syscall :
    int glvals -> BinNums.positive Syscall_t.syscall_t -> int gexprs -> data -> data

  val visit_assign : int glval -> E.assgn_tag -> int gty -> int gexpr -> data -> data

  val visit_copn : int glval list -> E.assgn_tag -> 'asm Sopn.sopn -> int gexprs -> data -> data

  val visit_for :
       ((annotation, 'asm) Prog.instr -> data -> data)
    -> int gvar_i
    -> int grange
    -> (annotation, 'asm) Prog.stmt
    -> data
    -> data

  val visit_while :
       ((annotation, 'asm) Prog.instr -> data -> data)
    -> E.align
    -> (annotation, 'asm) Prog.stmt
    -> int gexpr
    -> (annotation, 'asm) Prog.stmt
    -> data
    -> data

  val visit_if :
       ((annotation, 'asm) Prog.instr -> data -> data)
    -> int gexpr
    -> (annotation, 'asm) Prog.stmt
    -> (annotation, 'asm) Prog.stmt
    -> data
    -> data
end

module Visitor = struct
  module type S = sig
    type data

    type annotation

    val initial_state : data

    val visit_prog : (annotation, 'asm) Prog.prog -> data -> data
  end

  module Make (V : PartialVisitor) : S with type data = V.data and type annotation = V.annotation =
  struct
    type data = V.data

    type annotation = V.annotation

    let initial_state = V.initial_state

    let rec _visit_instr (instr : (int, V.annotation, 'asm) ginstr) (data : V.data) : V.data =
        match instr.i_desc with
        | Prog.Cassgn (l, a, ty, e) -> V.visit_assign l a ty e data
        | Prog.Copn (l, a, sopn, es) -> V.visit_copn l a sopn es data
        | Prog.Cif (e, s1, s2) -> V.visit_if _visit_instr e s1 s2 data
        | Prog.Cfor (i, r, s) -> V.visit_for _visit_instr i r s data
        | Prog.Cwhile (a, s1, e, s2) -> V.visit_while _visit_instr a s1 e s2 data
        | Prog.Ccall (l, f, es) -> V.visit_funcall l f es data
        | Prog.Csyscall (l, s, es) -> V.visit_syscall l s es data

    let visit_prog prog data : data = V.visit_prog _visit_instr prog data
  end
end

(*
   simple example of a visitor for copy-paste and avoiding rewriting
   module ExampleVisitor : PartialVisitor = struct
     type data = Sf.t

     type annotation = unit

     let inital_state : data = Sf.empty

     let visit_prog (prog : (annotation, 'asm) prog) data : data = data

     let visit_function (func : (annotation, 'asm) func) data : data = data

     let visit_stmt visit_instr stmt data : data = data

     let visit_funcall lvs funname parameters data : data = data

     let visit_syscall lvs syscall params data : data = data

     let visit_assign lv tag gty expr data : data = data

     let visit_copn lvs tag opn exprs data : data = data

     let visit_for visit_instr var range stmt data : data = data

     let visit_while visit_instr align b1 cond b2 data : data = data

     let visit_if visit_instr cond th el data : data = data
   end *)
