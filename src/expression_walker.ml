open Jasmin
open Prog

module type ExpressionState = sig
  type t

  val pconst : Z.t -> t

  val pbool : bool -> t

  val parr_init : int -> t

  val pvar : int ggvar -> t

  val pget : Memory_model.aligned -> Warray_.arr_access -> Wsize.wsize -> int ggvar -> t -> t

  val pload : Memory_model.aligned -> Wsize.wsize -> int gvar_i -> t -> t

  val psub : Warray_.arr_access -> Wsize.wsize -> int -> int ggvar -> t -> t

  val app1 : E.sop1 -> t -> t

  val app2 : E.sop2 -> t -> t -> t

  val appN : E.opN -> t list -> t

  val pif : 'len gty -> t -> t -> t -> t
end

module ExpressionWalker (State : ExpressionState) = struct
  type state = State.t

  let rec walk_expr (expr : int gexpr) : state =
      match expr with
      | Pconst c -> State.pconst c
      | Pbool b -> State.pbool b
      | Parr_init ws -> State.parr_init ws
      | Pvar var -> State.pvar var
      | Pget (al, acc, ws, var, expr) ->
          let state = walk_expr expr in
          State.pget al acc ws var state
      | Psub (al, ws, i, var, expr) ->
          let state = walk_expr expr in
          State.psub al ws i var state
      | Pload (al, ws, var, expr) ->
          let state = walk_expr expr in
          State.pload al ws var state
      | Papp1 (op, expr) -> State.app1 op (walk_expr expr)
      | Papp2 (op, l, r) -> State.app2 op (walk_expr l) (walk_expr r)
      | PappN (opn, exprs) -> State.appN opn (List.map walk_expr exprs)
      | Pif (ty, e1, e2, e3) -> State.pif ty (walk_expr e1) (walk_expr e2) (walk_expr e3)
end
