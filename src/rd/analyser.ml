open Jasmin
open Prog
open Analyser
open Domain

module ReachingDefinitionLogic : AnalyserLogic with type annotation = Domain.t = struct
  type annotation = Domain.t

  let initial_annotation = Domain.empty

  let fixpoint_condition prev state = Domain.included state prev

  let condition_split _ state = (state, state)

  let merge s1 s2 = Domain.join s1 s2

  let funcall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let syscall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let assign loc lv _ _ _ state = Domain.add (written_lv Sv.empty lv) (Instruction loc) state

  let opn loc lvs _ _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state
end

module ReachingDefinitionAnalyser : Analyser.S with type annotation = Domain.t =
  Analyser.Make (ReachingDefinitionLogic)
