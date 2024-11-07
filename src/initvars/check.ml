open Jasmin
open Utils
open Prog
open Rd.Mutator
open Types.Sgv
open Rd.Domain
open Error
open Types.Iloc

type ud_error = VarNotIntialized of int gvar_i

let is_local (v : int ggvar) =
    match v.gs with
    | Slocal -> true
    | _ -> false

let rec lvars_e (e : expr) : Sgv.t =
    match e with
    | Pconst _ -> Sgv.empty
    | Pbool _ -> Sgv.empty
    | Parr_init _ -> Sgv.empty
    | Pvar var ->
        if is_local var then
          Sgv.singleton var.gv
        else
          Sgv.empty
    | Pget (_, _, _, var, expr) ->
        let eset = lvars_e expr in
        if is_local var then
          Sgv.add var.gv eset
        else
          eset
    | Psub (_, _, _, var, expr) ->
        let eset = lvars_e expr in
        if is_local var then
          Sgv.add var.gv eset
        else
          eset
    | Pload (_, _, var, expr) ->
        let eset = lvars_e expr in
        Sgv.add var eset (*this var is always local*)
    | Papp1 (_, expr) -> lvars_e expr
    | Papp2 (_, l, r) -> Sgv.union (lvars_e l) (lvars_e r)
    | PappN (_, exprs) -> List.fold (fun sl e -> Sgv.union sl (lvars_e e)) Sgv.empty exprs
    | Pif (_, e1, e2, e3) -> Sgv.union (lvars_e e1) (Sgv.union (lvars_e e2) (lvars_e e3))

let check_ud_var (dom : Domain.t) (_ : L.i_loc) (modified_local_var : 'len gvar_i) : unit =
    match Mv.find_opt (L.unloc modified_local_var) dom with
    | None -> rs_uderror ~loc:(L.loc modified_local_var) (VarNotIntialized modified_local_var)
    | Some iset ->
        if Si.is_empty iset then
          assert false (*TODO : handle error here*)

let ud_expr (dom : Domain.t) (instr : L.i_loc) (locvars : Sv.t) (e : expr) : unit =
    let found_vars = lvars_e e in
    let used_local_vars =
        Sgv.fold
          (fun e sl ->
            if Sv.mem (L.unloc e) locvars then
              Sgv.add e sl
            else
              sl )
          found_vars Sgv.empty
    in
    Sgv.iter (check_ud_var dom instr) used_local_vars

let ud_range (dom : Domain.t) (locvars : Sv.t) (instr : L.i_loc) ((_, e1, e2) : 'len grange) : unit
    =
    ud_expr dom instr locvars e1 ; ud_expr dom instr locvars e2

let rec ud_instr (locvars : Sv.t) (instr : ('len, Domain.t, 'asm) ginstr) : unit =
    match instr.i_desc with
    | Cassgn (_, _, _, expr) -> ud_expr instr.i_info instr.i_loc locvars expr
    | Copn (_, _, _, exprs) -> List.iter (ud_expr instr.i_info instr.i_loc locvars) exprs
    | Csyscall (_, _, exprs) -> List.iter (ud_expr instr.i_info instr.i_loc locvars) exprs
    | Ccall (_, _, exprs) -> List.iter (ud_expr instr.i_info instr.i_loc locvars) exprs
    | Cif (expr, b1, b2) ->
        ud_expr instr.i_info instr.i_loc locvars expr ;
        ud_stmt locvars b1 ;
        ud_stmt locvars b2
    | Cfor (_, r, b) ->
        ud_range instr.i_info locvars instr.i_loc r ;
        ud_stmt locvars b
    | Cwhile (_, b1, c, b2) ->
        ud_expr instr.i_info instr.i_loc locvars c ;
        ud_stmt locvars b1 ;
        ud_stmt locvars b2

and ud_stmt (locvars : Sv.t) stmt : unit = List.iter (ud_instr locvars) stmt

let ud_func (f : ('info', 'asm) func) : unit =
    let f, _ = RdWalker.walk_func f Domain.empty in
    let locvars = Prog.locals f in
    ud_stmt locvars f.f_body

let ud_prog ((_, funcs) : ('info', 'asm) prog) : unit = List.iter ud_func funcs
