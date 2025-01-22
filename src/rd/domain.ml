open Jasmin
open Prog
open Srdi
open Utils
open Format

module Domain = struct
  type t = Srdi.t Mv.t

  let empty : t = Mv.empty

  let from_function_start (f : ('info, 'asm) func) : t =
      let locvars = Prog.locals f in
      Sv.fold (fun x acc -> Mv.add x (Srdi.singleton Default) acc) locvars Mv.empty

  let add xs i = Sv.fold (fun x -> Mv.add x (Srdi.singleton i)) xs

  let join = Mv.union (fun _ a b -> Some (Srdi.union a b))

  let included (x : t) (y : t) =
      Mv.for_all (fun x s1 -> Srdi.subset s1 (Mv.find_default Srdi.empty x y)) x

  let forget x = Mv.remove x

  let pp fmt ((_, d) : Location.i_loc * t) =
      Mv.iter
        (fun k v ->
          Format.fprintf fmt "%s : %a@." k.v_name
            (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") RdIloc.pp)
            (Srdi.to_list v) )
        d
end

let written_lv s = function
    | Lvar x -> Sv.add (L.unloc x) s
    | _ -> s

let written_lvs = List.fold_left written_lv Sv.empty

let print_domain (d : Domain.t) : unit =
    Mv.iter
      (fun x s ->
        Printf.printf "%s: %s \n" x.v_name
          (Srdi.fold
             (fun iset acc ->
               match iset with
               | Default -> acc ^ "Default "
               | Instruction i -> acc ^ "Instruction " ^ L.tostring i.base_loc )
             s "" ) )
      d
