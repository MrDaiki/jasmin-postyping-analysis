open Jasmin
open Prog

module Gv = struct
  type t = int gvar_i

  let compare (a : t) (b : t) = Stdlib.compare (L.unloc a).v_id (L.unloc b).v_id
end

module Sgv = Set.Make (Gv)
