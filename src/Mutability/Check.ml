open Jasmin
open Prog
open Utils
open Visitor.ProgramVisitor
open Mutable
open Error

let md_func_arg fname (arg : expr) (signature : var) =
    match arg with
    | Pconst _
     |Pbool _
     |Parr_init _
     |Pget _
     |Psub _
     |Pload _
     |Papp1 _
     |Papp2 _
     |PappN _
     |Pif _ ->
        ()
    | Pvar {gs= _; gv= v} -> (
        let var_mut = var_mutability (L.unloc v) in
        let sig_mut = var_mutability signature in
        print_mutability var_mut ;
        print_mutability sig_mut ;
        match (var_mut, sig_mut) with
        | Constant, Mutable ->
            rs_mderror ~loc:(L.loc v)
              (FunctionArgNotMutable (fname, signature.v_name, (L.unloc v).v_name))
        | _ -> () )

let md_func_args fname (arguments : expr list) (signatures : FunctionsArguments.t) =
    List.iter
      (fun (arg, s) -> md_func_arg fname arg s)
      (List.combine arguments (Mf.find fname signatures))

let md_lvalue (lv : lval) =
    match lv with
    | Lnone _
     |Lmem _
     |Lvar _ ->
        ()
    | Laset (_, _, _, v, _)
     |Lasub (_, _, _, v, _) ->
    match var_mutability (L.unloc v) with
    | Constant -> rs_mderror ~loc:(L.loc v) (AssignOnNonMutablePtr (L.unloc v).v_name)
    | _ -> ()

let md_lvalues (lvs : lval list) = List.iter md_lvalue lvs

let build_function_arguments funcs =
    List.fold_left
      (fun env func -> FunctionsArguments.add_function func env)
      FunctionsArguments.empty funcs

let build_visitor ((_, funcs) : ('info, 'asm) prog) :
    (module Visitor.S with type data = unit and type annotation = unit) =
    let function_arguments = build_function_arguments funcs in
    let module ExampleVisitor : PartialVisitor with type data = unit and type annotation = unit =
    struct
      type data = unit

      type annotation = unit

      let visit_funcall
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (funname : funname)
          (params : int gexprs)
          (_ : data) : data =
          md_lvalues lvs ;
          md_func_args funname params function_arguments

      let visit_syscall
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (_ : 'asm Syscall_t.syscall_t)
          (_ : int gexprs)
          (_ : data) : data =
          md_lvalues lvs

      let visit_assign
          (_ : L.i_loc)
          (_ : annotation)
          (lv : int glval)
          (_ : E.assgn_tag)
          (_ : int gty)
          (_ : int gexpr)
          (_ : data) : data =
          md_lvalue lv

      let visit_copn
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (_ : E.assgn_tag)
          (_ : 'asm Sopn.sopn)
          (_ : int gexprs)
          (_ : data) : data =
          md_lvalues lvs

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
          visit_stmt visit_instr b1 data ; visit_stmt visit_instr b2 data

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
          visit_stmt visit_instr func.f_body data

      let visit_prog
          (visit_instr : (annotation, 'asm) instr -> data -> data)
          ((_, funcs) : (annotation, 'asm) prog)
          data : data =
          List.iter (fun f -> ignore (visit_function visit_instr f data)) funcs ;
          data
    end in
    let module V = Visitor.Make (ExampleVisitor) in
    (module V : Visitor.S with type data = unit and type annotation = unit)

let md_prog prog =
    let (module V : Visitor.S with type data = unit and type annotation = unit) =
        build_visitor prog
    in
    V.visit_prog prog ()
