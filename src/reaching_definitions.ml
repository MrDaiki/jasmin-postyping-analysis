open Jasmin
open Utils
open Prog

open Walker

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

module Rdmutator : Statemutator = struct
  type state = Domain.t
  type annotation = Domain.t


  let loop_stop_condition prev state = Domain.included state prev

  let cif _ _ out1 out2 =
    let state = Domain.join out1 out2 in
    state, state

  let cfor _ _ _ prev state  =
    let state = Domain.join prev state in 
    state, state 

  let cfor_decl (lv: int gvar_i) _ loc state = 
    Domain.add (Sv.singleton lv.pl_desc) loc state
  
  let cwhile _ _ _ prev state = 
    let state = Domain.join prev state in 
    state, state

  let cassign  lv (_:E.assgn_tag) (_:('len gty)) (_:'len gexpr) (loc:L.i_loc) (state:state)  =
    let state = Domain.add (written_lv Sv.empty lv) loc state in 
    state,state
    
  let copn lvs _ _ _ loc state = 
    let state = Domain.add (written_lvs lvs) loc state in 
    state,state
  
  let fcall lvs (_:funname) (_: 'len gexprs) loc (state:state) =
    let state = Domain.add (written_lvs lvs) loc state in 
    state,state
  
  let syscall lvs _ _ loc state =
    let state = Domain.add (written_lvs lvs) loc state in 
    state,state

end


module RdWalker = TreeWalker(Rdmutator)