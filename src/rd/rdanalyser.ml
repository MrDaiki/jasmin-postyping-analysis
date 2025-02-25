open Jasmin
open Prog
open Domain
open Analyser

module ReachingDefinitionLogic :
  AnalyserLogic with type annotation = Domain.t and type domain = Domain.t = struct
  type domain = Domain.t

  type annotation = Domain.t

  let to_annotation in_domain _ = in_domain

  let pp_annot fmt = Domain.pp fmt

  let included prev state = Domain.included state prev

  let assume _ state = (state, state)

  let merge s1 s2 = Domain.join s1 s2

  let forget var state = Domain.forget (L.unloc var) state

  let funcall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let syscall loc lvs _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state

  let assign loc lv _ _ _ state = Domain.add (written_lv Sv.empty lv) (Instruction loc) state

  let opn loc lvs _ _ _ state = Domain.add (written_lvs lvs) (Instruction loc) state
end

module ReachingDefinitionAnalyser :
  ForwardAnalyser.S with type annotation = Domain.t and type domain = Domain.t =
  ForwardAnalyser.Make (ReachingDefinitionLogic)
