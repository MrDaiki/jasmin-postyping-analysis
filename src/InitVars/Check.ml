open Jasmin
open Prog
open Visitor
open ExpressionChecker
open Checker

module InitVarVisitor :
  ExpressionChecker.S with type self = iv_data and type annotation = Rd.Domain.t =
  ExpressionChecker.Make (InitVarCheckerLogic)

let get_all_locals funcs = List.fold_left (fun acc f -> Sv.union acc (Prog.locals f)) Sv.empty funcs

let initial_state funcs : iv_data = {locals= get_all_locals funcs; mode= NotStrict; errors= []}

let iv_prog ((globs, funcs) : ('info, 'asm) prog) (mode : check_mode) =
    let funcs =
        List.map
          (fun f ->
            let f =
                Rd.RdAnalyser.ReachingDefinitionAnalyser.analyse_function f
                  (Rd.Domain.from_function_start f)
            in
            f )
          funcs
    in
    let prog = (globs, funcs) in
    let data = {(initial_state funcs) with mode} in
    let data = InitVarVisitor.visit_prog prog data in
    (prog, List.rev data.errors)
