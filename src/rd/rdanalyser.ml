open Jasmin
open Prog
open Domain
open Analyser

module ReachingDefinitionLogic : AnalyserLogic with type annotation = Domain.t = struct
  type annotation = Domain.t

  let included prev state = Domain.included state prev

  let assume _ state = (state, state)

  let merge s1 s2 = Domain.join s1 s2

  let forget var state = Domain.forget (L.unloc var) state

  let funcall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let syscall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let assign loc lv _ _ _ state = Domain.add (written_lv Sv.empty lv) (Instruction loc) state

  let opn loc lvs _ _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state
end

module ReachingDefinitionAnalyser : TreeAnalyser.S with type annotation = Domain.t =
  TreeAnalyser.Make (ReachingDefinitionLogic)
