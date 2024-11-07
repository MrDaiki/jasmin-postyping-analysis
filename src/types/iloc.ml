open Jasmin

module ILoc = struct
  open Location

  type t = i_loc

  let compare x y = Stdlib.Int.compare x.uid_loc y.uid_loc
end

module Si = Set.Make (ILoc)
