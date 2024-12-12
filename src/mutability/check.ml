open Jasmin
open Prog
open Utils
open Visitor.Programvisitor
open Environment
open Error

let md_lvalue (variables : MutabilityEnvironment.t) (lv : 'len glval) : unit =
    match lv with
    | Lnone _ -> ()
    | Lmem (_, _, gv, _)
     |Laset (_, _, _, gv, _)
     |Lasub (_, _, _, gv, _)
     |Lvar gv ->
        if not (MutabilityEnvironment.check_mutable variables (L.unloc gv)) then
          rs_mderror ~loc:(L.loc gv) (AssignOnNonMutablePtr (L.unloc gv).v_name)

let md_lvalues (variables : MutabilityEnvironment.t) (lv : 'len glval list) : unit =
    List.iter (md_lvalue variables) lv

let md_func_args (variables : MutabilityEnvironment.t) (fun_name : funname) (args : 'len gexpr list)
    : unit =
    let rec cmp_mutability (fsig : (mutability * string) list) (args : expr list) : unit =
        match (fsig, args) with
        | (NotPtr, _) :: s, _ :: a -> cmp_mutability s a
        | (Mutable, param) :: s, arg :: a -> (
            match arg with
            | Pvar gv ->
                if not (MutabilityEnvironment.check_mutable variables (L.unloc gv.gv)) then
                  rs_mderror ~loc:(L.loc gv.gv)
                    (FunctionArgNotMutable (fun_name, param, (L.unloc gv.gv).v_name))
                else
                  cmp_mutability s a
            | _ -> assert false )
        | (Constant, _) :: s, _ :: a -> cmp_mutability s a
        | [], [] -> ()
        | _, []
         |[], _ ->
            assert false
        (*This error shoud never be raised, it correspond to call with the wrong number of argument, which is checked previously*)
    in
    cmp_mutability (Mf.find fun_name variables.functions) args

let populate_environment_globals (globs : global_decl list) =
    List.fold
      (fun env (v, _) -> MutabilityEnvironment.add_variable env v)
      MutabilityEnvironment.empty globs

let populate_environment_funcs (funcs : ('len, 'info, 'asm) gfunc list) env =
    List.fold
      (fun env func -> MutabilityEnvironment.add_function_arguments env func.f_name func.f_args)
      env funcs

let populate_env (globs, funcs) =
    let env = populate_environment_globals globs in
    populate_environment_funcs funcs env

let build_visitor prog : (module Visitor.S with type data = unit and type annotation = unit) =
    let env = populate_env prog in
    let module ExampleVisitor : PartialVisitor with type data = unit and type annotation = unit =
    struct
      type data = unit

      type annotation = unit

      let initial_state = ()

      let visit_funcall
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (funname : funname)
          (params : int gexprs)
          (_ : data) : data =
          md_lvalues env lvs ; md_func_args env funname params

      let visit_syscall
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (_ : 'asm Syscall_t.syscall_t)
          (_ : int gexprs)
          (_ : data) : data =
          md_lvalues env lvs

      let visit_assign
          (_ : L.i_loc)
          (_ : annotation)
          (lv : int glval)
          (_ : E.assgn_tag)
          (_ : int gty)
          (_ : int gexpr)
          (_ : data) : data =
          md_lvalue env lv

      let visit_copn
          (_ : L.i_loc)
          (_ : annotation)
          (lvs : int glvals)
          (_ : E.assgn_tag)
          (_ : 'asm Sopn.sopn)
          (_ : int gexprs)
          (_ : data) : data =
          md_lvalues env lvs

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
    V.visit_prog prog V.initial_state
