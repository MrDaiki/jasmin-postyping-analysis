open Jasmin
open Prog
open Analyser.BackwardAnalyser

let lv_dep domain lv =
    match lv with
    | Lvar var -> Sv.remove (L.unloc var) domain
    | _ -> vars_lv domain lv

let lvs_dep domain lvs = List.fold_left lv_dep domain lvs

let empty = Sv.empty

let merge t1 t2 = Sv.union t1 t2

let pp_sv fmt sv =
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt v -> Format.fprintf fmt "%s" v.v_name) )
      (Sv.to_list sv)

module LivenessDomain : BackwardAnalyserLogic with type annotation = Sv.t = struct
  type annotation = Sv.t

  let pp_annot (fmt : Format.formatter) ((_, domain) : L.i_loc * annotation) = pp_sv fmt domain

  let included a b = Sv.subset a b

  let account expr domain = Sv.fold (fun v dom -> Sv.add v dom) (Prog.vars_e expr) domain

  let merge = Sv.union

  let forget var domain = Sv.remove (L.unloc var) domain

  let funcall (_ : Location.i_loc) (lvs : lvals) (_ : funname) (exprs : exprs) domain =
      let domain = lvs_dep domain lvs in
      List.fold_left (fun dom e -> account e dom) domain exprs

  let lived_variables = List.fold_left (fun acc lv -> Sv.add (L.unloc lv) acc) Sv.empty

  let syscall
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (exprs : exprs)
      domain =
      let domain = lvs_dep domain lvs in
      List.fold_left (fun dom e -> account e dom) domain exprs

  let assign
      (_ : Location.i_loc)
      (lv : lval)
      (_ : E.assgn_tag)
      (_ : ty)
      (expr : expr)
      (domain : annotation) =
      let domain = lv_dep domain lv in
      account expr domain

  let opn
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (exprs : exprs)
      domain =
      let domain = lvs_dep domain lvs in
      List.fold_left (fun dom e -> account e dom) domain exprs
end

module LivenessAnalyser : BackwardAnalyser.S with type annotation = Sv.t =
  BackwardAnalyser.Make (LivenessDomain)
