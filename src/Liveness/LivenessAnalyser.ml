open Jasmin
open Prog
open Analyser.BackwardAnalyser

(**
Liveness analysis should compile living variable for each program point
*)

(* TODO : make 'left expressions' live *)
let lv_dep domain lv =
    match lv with
    | Lvar var -> Sv.remove (L.unloc var) domain
    | _ -> vars_lv domain lv

let lvs_dep domain lvs = List.fold_left lv_dep domain lvs

let empty = Sv.empty

let merge t1 t2 = Sv.union t1 t2

let pp_sv fmt sv =
    Format.fprintf fmt "{ %a }\n"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt v -> Format.fprintf fmt "%s" v.v_name) )
      (Sv.to_list sv)

let live_assigns domain lvs exprs =
    let domain = lvs_dep domain lvs in
    Sv.union domain (Prog.vars_es exprs)

module LivenessDomain : BackwardAnalyserLogic with type annotation = Sv.t = struct
  type annotation = Sv.t

  let empty = Sv.empty

  let pp_annot (fmt : Format.formatter) ((_, domain) : L.i_loc * annotation) = pp_sv fmt domain

  let included a b = Sv.subset a b

  let account expr d1 d2 = Sv.union (Prog.vars_e expr) (Sv.union d1 d2)

  let forget var domain =
      assert (not (Sv.mem (L.unloc var) domain)) ;
      domain

  let funcall (_ : Location.i_loc) (lvs : lvals) (_ : funname) (exprs : exprs) domain =
      live_assigns domain lvs exprs

  let syscall
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : BinNums.positive Syscall_t.syscall_t)
      (exprs : exprs)
      domain =
      live_assigns domain lvs exprs

  let assign
      (_ : Location.i_loc)
      (lv : lval)
      (_ : E.assgn_tag)
      (_ : ty)
      (expr : expr)
      (domain : annotation) =
      live_assigns domain [lv] [expr]

  let opn
      (_ : Location.i_loc)
      (lvs : lvals)
      (_ : E.assgn_tag)
      (_ : 'asm Sopn.sopn)
      (exprs : exprs)
      domain =
      live_assigns domain lvs exprs

  let initialize fd = Sv.of_list (List.map L.unloc fd.f_ret)
end

module LivenessAnalyser : BackwardAnalyser.S with type annotation = Sv.t =
  BackwardAnalyser.Make (LivenessDomain)
