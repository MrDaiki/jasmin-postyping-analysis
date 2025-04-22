module ReachingDefinitionLogic : sig
  type domain = Rd.Domain.t

  val initialize : ('info, 'asm) Jasmin.Prog.func -> domain Analyser.Annotation.annotation

  val pp : Format.formatter -> Jasmin.Location.i_loc * domain -> unit

  val included : domain -> domain -> bool

  val assume :
       Jasmin.Prog.expr
    -> domain Analyser.Annotation.annotation
    -> domain Analyser.Annotation.annotation * domain Analyser.Annotation.annotation

  val merge : domain -> domain -> domain

  val forget : Jasmin.Prog.var_i -> domain -> domain Analyser.Annotation.annotation

  val funcall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.CoreIdent.funname
    -> Jasmin.Prog.exprs
    -> domain
    -> domain Analyser.Annotation.annotation

  val syscall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.BinNums.positive Jasmin.Syscall_t.syscall_t
    -> Jasmin.Prog.exprs
    -> domain
    -> domain Analyser.Annotation.annotation

  val assign :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lval
    -> Jasmin.Expr.assgn_tag
    -> Jasmin.Prog.ty
    -> Jasmin.Prog.expr
    -> domain
    -> domain Analyser.Annotation.annotation

  val opn :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.Expr.assgn_tag
    -> 'asm Jasmin.Sopn.sopn
    -> Jasmin.Prog.exprs
    -> domain
    -> domain Analyser.Annotation.annotation
end

module ReachingDefinitionAnalyser : sig
  type domain = ReachingDefinitionLogic.domain

  val analyse_function :
    ('info, 'asm) Jasmin.Prog.func -> (domain Analyser.Annotation.annotation, 'asm) Jasmin.Prog.func
end
