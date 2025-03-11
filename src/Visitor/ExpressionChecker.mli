module type ExpressionCheckerLogic = sig
  type domain

  type self

  val check_expr : self -> domain -> Jasmin.Location.i_loc -> Jasmin.Prog.expr -> self

  val check_return_variable : self -> domain -> Jasmin.Prog.var_i -> self

  val check_lv_variable : self -> domain -> Jasmin.Prog.var_i -> self
end

module ExpressionChecker : sig
  module Make : functor (Logic : ExpressionCheckerLogic) -> sig
    type annotation = Logic.domain

    type data = Logic.self

    val visit_prog : (annotation, 'asm) Jasmin.Prog.prog -> data -> data
  end
end
