module ReachingDefinitionLogic : sig
  type domain = Domain.t

  type annotation =
  | Empty
  | Annotation of domain

  val pp_annot : Format.formatter -> Jasmin.Location.i_loc * annotation -> unit

  val included : domain -> domain -> bool

  val assume : Jasmin.Prog.expr -> domain -> annotation * annotation

  val merge : domain -> domain -> domain

  val forget : domain -> Jasmin.Prog.var_i -> annotation

  val funcall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.CoreIdent.funname
    -> Jasmin.Prog.exprs
    -> annotation
    -> annotation

  val syscall :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.BinNums.positive Jasmin.Syscall_t.syscall_t
    -> Jasmin.Prog.exprs
    -> annotation
    -> annotation

  val assign :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lval
    -> Jasmin.Expr.assgn_tag
    -> Jasmin.Prog.ty
    -> Jasmin.Prog.expr
    -> annotation
    -> annotation

  val opn :
       Jasmin.Location.i_loc
    -> Jasmin.Prog.lvals
    -> Jasmin.Expr.assgn_tag
    -> 'asm Jasmin.Sopn.sopn
    -> Jasmin.Prog.exprs
    -> annotation
    -> annotation
end

module ReachingDefinitionAnalyser : sig
  type annotation = ReachingDefinitionLogic.annotation

  val analyse_function :
    ('info, 'asm) Jasmin.Prog.func -> annotation -> (annotation, 'asm) Jasmin.Prog.func
end
