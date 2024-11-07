open Jasmin
open Prog
open Types.Iloc

module Domain = struct
  type t = Si.t Mv.t

  let empty : t = Mv.empty

  let add xs i = Sv.fold (fun x -> Mv.add x (Si.singleton i)) xs

  let join = Mv.union (fun _ a b -> Some (Si.union a b))

  let included (x : t) (y : t) =
      Mv.for_all (fun x s1 -> Si.subset s1 (Mv.find_default Si.empty x y)) x
end

let written_lv s = function
    | Lvar x -> Sv.add (L.unloc x) s
    | _ -> s

let written_lvs = List.fold_left written_lv Sv.empty
