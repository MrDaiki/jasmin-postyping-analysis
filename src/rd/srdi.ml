open Jasmin
open Utils
open Location

type rd_instruction =
| Default
| Instruction of i_loc

module RdIloc = struct
  type t = rd_instruction

  let compare x y =
      match (x, y) with
      | Default, Default -> 0
      | Default, _ -> -1
      | _, Default -> 1
      | Instruction x, Instruction y -> Stdlib.Int.compare x.uid_loc y.uid_loc

  let pp fmt = function
      | Default -> Format.fprintf fmt "?"
      | Instruction i -> Format.fprintf fmt "%a" pp_iloc_short i
end

module Srdi = Set.Make (RdIloc)
