open Jasmin
open Prog
open Analyser.BackwardAnalyser
open Analyser.Annotation

(***
Dead code : 

if you assign a dead variable, you should raise a warning
=> opn + syscall => dead code doesn't erase them

*)
let lv_dep domain lv =
    match lv with
    | Lvar var -> Sv.remove (L.unloc var) domain
    | _ -> vars_lv domain lv

let lvs_dep domain lvs = List.fold_left lv_dep domain lvs

let empty = Sv.empty

let merge t1 t2 = Sv.union t1 t2

let pp_sv fmt (_, sv) =
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt v -> Format.fprintf fmt "%s" v.v_name) )
      (Sv.to_list sv)

module LivenessDomain : BackwardAnalyserLogic with type domain = Sv.t = struct
  type domain = Sv.t

  let unwrap annotation =
      match annotation with
      | Empty -> Sv.empty
      | Annotation d -> d

  let dead_variables domain expr = Sv.union domain (Prog.vars_e expr)

  let pp_annot (fmt : Format.formatter) (annot : L.i_loc * domain annotation) =
      pp_annotation pp_sv fmt annot

  let included a b = Sv.subset b a

  let account expr a1 a2 =
      match (a1, a2) with
      | Empty, _ -> a2
      | _, Empty -> a1
      | Annotation d1, Annotation d2 -> Annotation (Sv.union (Prog.vars_e expr) (Sv.union d1 d2))

  let forget var domain = Sv.remove (L.unloc var) domain

  let funcall (_ : Location.i_loc) (lvs : lvals) (_ : funname) (exprs : exprs) annotation =
      let domain = lvs_dep (unwrap annotation) lvs in
      Annotation (List.fold_left (fun dom e -> dead_variables dom e) domain exprs)

  let syscall
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (exprs : exprs)
      annotation =
      let domain = lvs_dep (unwrap annotation) lvs in
      Annotation (List.fold_left (fun dom e -> dead_variables dom e) domain exprs)

  let assign
      (_ : Location.i_loc)
      (lv : lval)
      (_ : E.assgn_tag)
      (_ : ty)
      (expr : expr)
      (annotation : domain annotation) =
      let domain = lv_dep (unwrap annotation) lv in
      Annotation (dead_variables domain expr)

  let opn
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (exprs : exprs)
      annotation =
      let domain = lvs_dep (unwrap annotation) lvs in
      Annotation (List.fold_left (fun dom e -> dead_variables dom e) domain exprs)
end

module LivenessAnalyser : BackwardAnalyser.S with type domain = Sv.t =
  BackwardAnalyser.Make (LivenessDomain)
