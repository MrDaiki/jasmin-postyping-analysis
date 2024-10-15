open Jasmin
open Utils
open Prog

module ILoc = struct
  open Location

  type t = i_loc

  let compare x y = Stdlib.Int.compare x.uid_loc y.uid_loc
end

module Si = Set.Make (ILoc)

module Domain = struct
  type t = Si.t Mv.t

  let empty : t = Mv.empty
  let add xs i = Sv.fold (fun x -> Mv.add x (Si.singleton i)) xs
  let join = Mv.union (fun _ a b -> Some (Si.union a b))

  let included (x : t) (y : t) =
    Mv.for_all (fun x s1 -> Si.subset s1 (Mv.find_default Si.empty x y)) x
end

let written_lv s = function Lvar x -> Sv.add (L.unloc x) s | _ -> s
let written_lvs = List.fold_left written_lv Sv.empty

let rec rd_instr acc i =
  let out, i_desc = rd_instr_r i.i_loc acc i.i_desc in
  (out, { i with i_desc; i_info = acc })

and rd_instr_r loc prev = function
  | Cif (e, th, el) ->
      let out1, th = rd_stmt prev th in
      let out2, el = rd_stmt prev el in
      (Domain.join out1 out2, Cif (e, th, el))
  | Cfor (x, rg, body) ->
      let prev = Domain.add (Sv.singleton x.pl_desc) loc prev in
      let out, body =
        let rec loop prev =
          let next, body = rd_stmt prev body in
          if Domain.included next prev then (prev, body)
          else loop (Domain.join prev next)
        in
        loop prev
      in
      (out, Cfor (x, rg, body))
  | Cwhile (al, body1, e, body2) ->
      let acc, _ = rd_stmt prev body1 in
      let out, body1, body2 =
        let rec loop prev =
          let acc, body2 = rd_stmt prev body2 in
          let next, body1 = rd_stmt acc body1 in
          if Domain.included next prev then (prev, body1, body2)
          else loop (Domain.join prev next)
        in
        loop acc
      in
      (out, Cwhile (al, body1, e, body2))
  | Cassgn (x, tg, ty, e) ->
      (Domain.add (written_lv Sv.empty x) loc prev, Cassgn (x, tg, ty, e))
  | Copn (xs, tg, op, es) ->
      (Domain.add (written_lvs xs) loc prev, Copn (xs, tg, op, es))
  | Csyscall (xs, op, es) ->
      (Domain.add (written_lvs xs) loc prev, Csyscall (xs, op, es))
  | Ccall (ii, xs, fn, es) ->
      (Domain.add (written_lvs xs) loc prev, Ccall (ii, xs, fn, es))

and rd_stmt acc = List.fold_left_map rd_instr acc

let rd_fundef fd = rd_stmt Domain.empty fd.f_body
