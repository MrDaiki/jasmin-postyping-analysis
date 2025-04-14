open Jasmin
open Prog
open Domain
open Analyser.ForwardAnalyser
open Analyser.Annotation

module ReachingDefinitionLogic : ForwardAnalyserLogic with type domain = Domain.t = struct
  type domain = Domain.t

  let unwrap annot =
      match annot with
      | Empty -> assert false
      | Annotation domain -> domain

  let pp_annot fmt (loc, annotation) =
      let domain = unwrap annotation in
      Domain.pp fmt (loc, domain)

  let included prev state = Domain.included state prev

  let assume _ state = (Annotation state, Annotation state)

  let merge s1 s2 = Domain.join s1 s2

  let forget state var = Annotation (Domain.forget (L.unloc var) state)

  let funcall loc lvs _ _ annotation =
      let domain = unwrap annotation in
      Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)

  let syscall loc lvs _ _ annotation =
      let domain = unwrap annotation in
      Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)

  let assign loc lv _ _ _ annotation =
      let domain = unwrap annotation in
      Annotation (Domain.add (written_lv Sv.empty lv) (Instruction loc) domain)

  let opn loc lvs _ _ _ annotation =
      let domain = unwrap annotation in
      Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)
end

module ReachingDefinitionAnalyser :
  ForwardAnalyser.S with type domain = ReachingDefinitionLogic.domain =
  ForwardAnalyser.Make (ReachingDefinitionLogic)
