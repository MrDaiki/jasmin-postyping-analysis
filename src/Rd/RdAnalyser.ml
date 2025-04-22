open Jasmin
open Prog
open Domain
open Analyser.ForwardAnalyser
open Analyser.Annotation
open Types

module ReachingDefinitionLogic : ForwardAnalyserLogic with type domain = Domain.t = struct
  type domain = Domain.t

  let initialize f =
      let locvars = Prog.locals f in
      let s = Sv.fold (fun x acc -> Mv.add x (SIloc.singleton Default) acc) locvars Mv.empty in
      Annotation
        (Sv.fold
           (fun x acc ->
             Mv.add x (SIloc.singleton (Instruction (Jasmin.Location.i_loc0 f.f_loc))) acc )
           (params f) s )

  let pp fmt = Domain.pp fmt

  (* Check if a is included in b*)
  let included a b = Domain.included a b

  let assume _ state = (state, state)

  let merge s1 s2 = Domain.join s1 s2

  let forget var domain = Annotation (Domain.forget (L.unloc var) domain)

  let funcall loc lvs _ _ domain =
      Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)

  let syscall loc lvs _ _ domain =
      Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)

  let assign loc lv _ _ _ domain =
      Annotation (Domain.add (written_lv Sv.empty lv) (Instruction loc) domain)

  let opn loc lvs _ _ _ domain = Annotation (Domain.add (written_lvs lvs) (Instruction loc) domain)
end

module ReachingDefinitionAnalyser : ForwardAnalyser.S with type domain = Domain.t =
  ForwardAnalyser.Make (ReachingDefinitionLogic)
