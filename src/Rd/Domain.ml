open Jasmin
open Prog
open Utils
open Format
open Types

type t = SIloc.t Mv.t

let empty : t = Mv.empty

(* TODO : remove when integrated in Prog.mli*)
let params fc = List.fold_left (fun s v -> Sv.add v s) Sv.empty fc.f_args

let from_function_start (f : ('info, 'asm) func) : t =
    let locvars = Prog.locals f in
    let s = Sv.fold (fun x acc -> Mv.add x (SIloc.singleton Default) acc) locvars Mv.empty in
    Sv.fold
      (fun x acc -> Mv.add x (SIloc.singleton (Instruction (Jasmin.Location.i_loc0 f.f_loc))) acc)
      (params f) s

let add xs i = Sv.fold (fun x -> Mv.add x (SIloc.singleton i)) xs

let join = Mv.union (fun _ a b -> Some (SIloc.union a b))

let included (x : t) (y : t) =
    Mv.for_all (fun x s1 -> SIloc.subset s1 (Mv.find_default SIloc.empty x y)) x

let forget x = Mv.remove x

let pp fmt ((_, d) : Location.i_loc * t) =
    Mv.iter
      (fun k v ->
        Format.fprintf fmt "%s : %a@." k.v_name
          (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Iloc.pp)
          (SIloc.to_list v) )
      d

(* TODO : remove when next release add it to prog.mli*)
let written_lv s = function
    | Lvar x -> Sv.add (L.unloc x) s
    | _ -> s

(* TODO : remove when next release add it to prog.mli*)
let written_lvs = List.fold_left written_lv Sv.empty
