open Jasmin
open Utils
open Prog

module Instr = struct
  type t = (unit, unit) instr

  let compare x y = Stdlib.Int.compare x.i_loc.uid_loc y.i_loc.uid_loc
end

module Si = Set.Make (Instr)
module Mi = Map.Make (Instr)

module Domain = struct
  type t = Si.t Mv.t

  let empty : t = Mv.empty
  let add xs i = Sv.fold (fun x -> Mv.add x (Si.singleton i)) xs
  let join = Mv.union (fun _ a b -> Some (Si.union a b))

  let included (x : t) (y : t) =
    Mv.for_all (fun x s1 -> Si.subset s1 (Mv.find_default Si.empty x y)) x
end

module Rd = struct
  type t = Domain.t Mi.t

  let empty : t = Mi.empty

  let included x y =
    Mi.for_all
      (fun i d1 ->
        match Mi.find i y with
        | exception Not_found -> false
        | d2 -> Domain.included d1 d2)
      x

  let join = Mi.union (fun _ a b -> Some (Domain.join a b))
end

module Acc = struct
  type t = Rd.t * Domain.t

  let empty : t = (Rd.empty, Domain.empty)

  let included (rd1, out1) (rd2, out2) =
    Domain.included out1 out2
    && (* conjecture: this second half is redundant *)
    Rd.included rd1 rd2

  let join (rd1, out1) (rd2, out2) = (Rd.join rd1 rd2, Domain.join out1 out2)

  let loop trans start =
    let rec fix (prev : t) : t =
      let next = trans prev in
      if included next prev then prev else fix (join prev next)
    in
    fix start
end

let written_lv s = function Lvar x -> Sv.add (L.unloc x) s | _ -> s

let written_vars = function
  | Cassgn (x, _, _, _) -> written_lv Sv.empty x
  | Copn (xs, _, _, _) | Csyscall (xs, _, _) | Ccall (xs, _, _) ->
      List.fold_left written_lv Sv.empty xs
  | Cif _ | Cfor _ | Cwhile _ -> assert false

let rec rd_instr ((_, prev) as acc) i =
  let rd, out = rd_instr_r i acc i.i_desc in
  (Mi.modify_def Domain.empty i (Domain.join prev) rd, out)

and rd_instr_r i ((rd, prev) as acc) = function
  | Cif (_, th, el) -> Acc.join (rd_stmt acc th) (rd_stmt acc el)
  | Cfor ({ pl_desc = x; _ }, _, body) ->
      let prev = Domain.add (Sv.singleton x) i prev in
      Acc.loop (fun acc -> rd_stmt acc body) (rd, prev)
  | Cwhile (_, body1, _, body2) ->
      let acc = rd_stmt acc body1 in
      Acc.loop (fun acc -> rd_stmt acc (body2 @ body1)) acc
  | x ->
      let defs = written_vars x in
      (rd, Domain.add defs i prev)

and rd_stmt acc = List.fold_left rd_instr acc

let rd_fundef fd = rd_stmt Acc.empty fd.f_body
