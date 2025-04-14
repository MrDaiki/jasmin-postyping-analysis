open Analyser.Annotation

module ReachingDefinitionLogic : sig
  type domain = Domain.t

  val pp_annot : Format.formatter -> Jasmin.Location.i_loc * domain annotation -> unit

  val included : domain -> domain -> bool

  val assume : Jasmin.Prog.expr -> domain -> domain annotation * domain annotation

  val merge : domain -> domain -> domain

  val forget : domain -> Jasmin.Prog.var_i -> domain annotation

  val funcall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.CoreIdent.funname
    -> Jasmin.Prog.exprs
    -> domain annotation
    -> domain annotation

  val syscall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.BinNums.positive Jasmin.Syscall_t.syscall_t
    -> Jasmin.Prog.exprs
    -> domain annotation
    -> domain annotation

  val assign :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lval
    -> Jasmin.Expr.assgn_tag
    -> Jasmin.Prog.ty
    -> Jasmin.Prog.expr
    -> domain annotation
    -> domain annotation

  val opn :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.Expr.assgn_tag
    -> 'asm Jasmin.Sopn.sopn
    -> Jasmin.Prog.exprs
    -> domain annotation
    -> domain annotation
end

module ReachingDefinitionAnalyser : sig
  type domain = ReachingDefinitionLogic.domain

  val analyse_function :
       ('info, 'asm) Jasmin.Prog.func
    -> domain annotation
    -> (domain annotation, 'asm) Jasmin.Prog.func
end
