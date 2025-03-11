module type ExpressionCheckerLogic = sig
  type domain

  type self

  val check_expr : self -> domain -> Jasmin.Location.i_loc -> Jasmin.Prog.expr -> self

  val check_return_variable : self -> domain -> Jasmin.Prog.var_i -> self

  val check_lv_variable : self -> domain -> Jasmin.Prog.var_i -> self
end

module ExpressionChecker : sig
  module Make : functor (Logic : ExpressionCheckerLogic) ->
    ProgramVisitor.Visitor.S with type data = Logic.self and type annotation = Logic.domain
end
