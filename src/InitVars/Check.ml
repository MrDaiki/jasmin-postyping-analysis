open Jasmin
open Prog
open Visitor
open ExpressionChecker
open Checker

module InitVarVisitor :
  ProgramVisitor.Visitor.S
    with type data = iv_data
     and type annotation = Rd.Domain.t Analyser.Annotation.annotation =
  ExpressionChecker.Make (InitVarCheckerLogic)

let initial_state : iv_data = {mode= NotStrict; errors= []}

let iv_prog ((globs, funcs) : ('info, 'asm) prog) (mode : check_mode) =
    let funcs =
        List.map
          (fun f ->
            let f = Rd.RdAnalyser.ReachingDefinitionAnalyser.analyse_function f in
            f )
          funcs
    in
    let prog = (globs, funcs) in
    let data = {initial_state with mode} in
    let data = InitVarVisitor.visit_prog prog data in
    (prog, List.rev data.errors)
