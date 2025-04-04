open Jasmin
open Prog
open Types

module type BackwardAnalyserLogic = sig
  (** type of annotation for the program*)
  type annotation

  (** Pretty printing function*)
  val pp_annot : Format.formatter -> Location.i_loc * annotation -> unit

  (** Incusion test
     Check if an annotation is included in another one.

     args :
     - prev : previous domain
     - domain : current domain

     returns :
     - bool : true if the fixpoint is reached, false otherwise

    Inclusion : for all A1, A2 , (included A1 A2) => for all s , (s in I(A2) => s in I(A1)) 
  *)

  val empty : annotation

  val included : annotation -> annotation -> bool

  (**
  
    Account : for all A1, A2, e, s , s in I(account e A1 A2) => if [e]s then s in I(A1) else s in I(A2)
    eq : for all A1, A2, e, s , s in I(account e A1 A2) => s in I(if [e]s then A1 else A2)
  *)
  val account : int gexpr -> annotation -> annotation -> annotation

  (** Function that remove a proxy variable from the annotation if necessary
       Some part of the tree walking will introduce proxy variables to handle some specific cases.
       This function is called to remove them from the annotation when they are not needed anymore
     *)
  val forget : int gvar_i -> annotation -> annotation

  val funcall : Location.i_loc -> int glvals -> funname -> int gexprs -> annotation -> annotation

  val syscall :
       Location.i_loc
    -> int glvals
    -> BinNums.positive Syscall_t.syscall_t
    -> int gexprs
    -> annotation
    -> annotation

  val assign :
    Location.i_loc -> int glval -> E.assgn_tag -> int gty -> int gexpr -> annotation -> annotation

  val opn :
       Location.i_loc
    -> int glval list
    -> E.assgn_tag
    -> 'asm Sopn.sopn
    -> int gexprs
    -> annotation
    -> annotation
end

module BackwardAnalyser = struct
  module type S = sig
    (** anotation type*)
    type annotation

    (**
       Entrypoint for analysis. Using domain, the domain for the previous instruction is computed. Annotation are then generated using `AnalyserLogic.to_annotation` function

       args :
       - Prog.func (function to analyse)
       - annotation (initial annotation that will be modified during analysis)

       returns :
       - Prog.func (annotated function)
       - annotation (out annotation)
    *)
    val analyse_function : ('info, 'asm) Prog.func -> annotation -> (annotation, 'asm) Prog.func
  end

  (** Functor used to build TreeAnalyser modules*)
  module Make
      (L : BackwardAnalyserLogic)
  (*: S with type annotation = L.annotation and type domain = L.domain  *) =
  struct
    type annotation = L.annotation

    let analyse_assign (loc : Location.i_loc) (lv : int glval) tag ty (expr : int gexpr) domain =
        let domain = L.assign loc lv tag ty expr domain in
        (Cassgn (lv, tag, ty, expr), domain)

    let build_for_proxy_variable loc (x : int gvar_i) : var_i =
        Location.mk_loc loc (GV.clone (Location.unloc x))

    let build_for_assign_expr (x : int gvar_i) (r : int grange) : int gexpr =
        let assign_op = Grange.incr_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (assign_op, Pvar x_ggvar, Pconst (Z.of_int 1))

    let build_for_condition_expr (x : int gvar_i) (r : int grange) : int gexpr =
        let gend = Grange.last r in
        let comp_op = Grange.cmp_operator r in
        let x_ggvar = {gv= x; gs= Slocal} in
        Papp2 (comp_op, Pvar x_ggvar, gend)

    let rec analyse_for
        (loc : Location.i_loc)
        variable
        (range : int grange)
        (body : ('info, 'asm) stmt)
        out_domain : (int, annotation, 'asm) ginstr_r * annotation =
        let proxy_var = build_for_proxy_variable loc.base_loc variable in
        let condition = build_for_condition_expr proxy_var range in
        let rec loop in_domain =
            let _, domain =
                (* Incrementing loop counter (proxy_var (+|-)= 1) *)
                analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
                  (build_for_assign_expr proxy_var range)
                  in_domain
            in
            let body, domain = analyse_stmt body domain in
            let _, domain =
                (* Assigning proxy_var to for variable*)
                analyse_assign loc (Lvar variable) AT_none (Location.unloc variable).v_ty
                  (Pvar {gv= proxy_var; gs= Slocal})
                  domain
            in
            let domain = L.account condition domain in_domain in
            (* Check if the loop is finished *)
            if L.included domain in_domain then
              (Cfor (variable, range, body), domain)
            else
              loop domain
        in
        let body, domain = loop out_domain in
        let _, domain =
            (* Assigning proxy_var to range beginning*)
            analyse_assign loc (Lvar proxy_var) AT_none (Location.unloc variable).v_ty
              (Grange.first range) domain
        in
        let domain = L.forget proxy_var domain in
        (body, domain)

    and analyse_while
        (al : E.align)
        (cond : int gexpr)
        ((a, _) : IInfo.t * 'info)
        (b1 : (int, 'info, 'asm) gstmt)
        (b2 : (int, 'info, 'asm) gstmt)
        (out_domain : annotation) =
        (*
        Invariant : L.included out_domain cond_out_domain
        *)
        let domain = L.account cond out_domain L.empty in
        (* Incrementing loop counter (proxy_var (+|-)= 1) *)
        let rec loop (cond_out_domain : annotation) =
            let b1, domain_b1 = analyse_stmt b1 cond_out_domain in
            let b2, domain_b2 = analyse_stmt b2 domain_b1 in
            let domain = L.account cond domain_b2 cond_out_domain in
            if L.included domain cond_out_domain then
              (Cwhile (al, b1, cond, (a, domain), b2), domain_b1)
            else
              loop domain
        in
        loop domain

    and analyse_instr_r
        (loc : Location.i_loc)
        (instr : (int, 'info, 'asm) ginstr_r)
        (domain : annotation) : (int, annotation, 'asm) ginstr_r * annotation =
        match instr with
        | Cassgn (lv, tag, ty, expr) -> analyse_assign loc lv tag ty expr domain
        | Copn (lvs, tag, sopn, es) ->
            let domain = L.opn loc lvs tag sopn es domain in
            (Copn (lvs, tag, sopn, es), domain)
        | Ccall (lvs, fn, es) ->
            let domain = L.funcall loc lvs fn es domain in
            (Ccall (lvs, fn, es), domain)
        | Csyscall (lvs, sc, es) ->
            let domain = L.syscall loc lvs sc es domain in
            (Csyscall (lvs, sc, es), domain)
        | Cif (cond, th, el) ->
            let th, domain_th = analyse_stmt th domain in
            let el, domain_el = analyse_stmt el domain in
            (Cif (cond, th, el), L.account cond domain_th domain_el)
        | Cfor (var, range, bloc) -> analyse_for loc var range bloc domain
        | Cwhile (align, b1, cond, info, b2) -> analyse_while align cond info b1 b2 domain

    and analyse_instr (in_domain : annotation) (instr : ('info, 'asm) instr) :
        annotation * (annotation, 'asm) instr =
        let instr_r, out_domain = analyse_instr_r instr.i_loc instr.i_desc in_domain in
        ( out_domain
        , {i_desc= instr_r; i_loc= instr.i_loc; i_info= in_domain; i_annot= instr.i_annot} )

    and analyse_stmt (stmt : ('info, 'asm) stmt) domain =
        let out_domain, stmt =
            List.fold_right
              (fun instr (back_domain, acc) ->
                let out_domain, instr = analyse_instr back_domain instr in
                (out_domain, instr :: acc) )
              (List.rev stmt) (domain, [])
        in
        (stmt, out_domain)

    let analyse_function (func : ('info, 'asm) Prog.func) (in_domain : annotation) :
        (annotation, 'asm) Prog.func =
        let body, out_domain = analyse_stmt func.f_body in_domain in
        { f_loc= func.f_loc
        ; f_annot= func.f_annot
        ; f_cc= func.f_cc
        ; f_info= out_domain
        ; f_name= func.f_name
        ; f_tyin= func.f_tyin
        ; f_args= func.f_args
        ; f_body= body
        ; f_tyout= func.f_tyout
        ; f_outannot= func.f_outannot
        ; f_ret= func.f_ret }
  end
end
