open Jasmin
open MemoryEffect
open Utils
open Prog
open Visitor.ProgramVisitor

let memory_effect_lv (lv : int glval) : memory_effect =
    match lv with
    | Lmem _ -> Some
    | _ -> None

let memory_effect_lvs (lvs : int glvals) : memory_effect =
    List.fold_left (fun acc lv -> acc || memory_effect_lv lv) None lvs

type memory_effect_visitor_data =
{ memory_effect: memory_effect Mf.t
; local_effect: memory_effect }

module MemoryEffectPartialVisitor :
  PartialVisitor with type data = memory_effect_visitor_data and type annotation = unit = struct
  type data = memory_effect_visitor_data

  type annotation = unit

  let visit_funcall
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (funname : funname)
      (_ : int gexprs)
      (data : data) : data =
      { data with
        local_effect= data.local_effect || memory_effect_lvs lvs || Depends (Sf.singleton funname)
      }

  let visit_syscall
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (_ : 'asm Syscall_t.syscall_t)
      (_ : int gexprs)
      (data : data) : data =
      {data with local_effect= data.local_effect || memory_effect_lvs lvs}

  let visit_assign
      (_ : L.i_loc)
      (_ : annotation)
      (lv : int glval)
      (_ : E.assgn_tag)
      (_ : int gty)
      (_ : int gexpr)
      (data : data) : data =
      {data with local_effect= data.local_effect || memory_effect_lv lv}

  let visit_copn
      (_ : L.i_loc)
      (_ : annotation)
      (lvs : int glvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (_ : int gexprs)
      (data : data) : data =
      {data with local_effect= data.local_effect || memory_effect_lvs lvs}

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
      List.fold_left (fun acc instr -> visit_instr instr acc) data stmt

  let visit_function
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      (func : (annotation, 'asm) func)
      data : data =
      let data = {data with local_effect= None} in
      let data = visit_stmt visit_instr func.f_body data in
      {data with memory_effect= Mf.add func.f_name data.local_effect data.memory_effect}

  let visit_prog
      (visit_instr : (annotation, 'asm) instr -> data -> data)
      ((_, funcs) : (annotation, 'asm) prog)
      data : data =
      List.fold_left (fun acc func -> visit_function visit_instr func acc) data funcs
end

module MemoryEffectVisitor :
  Visitor.S with type data = memory_effect_visitor_data and type annotation = unit =
  Visitor.Make (MemoryEffectPartialVisitor)

let initial_state = {memory_effect= Mf.empty; local_effect= None}

let memory_effects (prog : ('info, 'asm) prog) : memory_effect Mf.t =
    (MemoryEffectVisitor.visit_prog prog initial_state).memory_effect
