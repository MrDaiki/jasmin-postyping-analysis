open Jasmin
open Prog
open Domain
open Analyser

module SignAnalyserLogic : AnalyserLogic with type annotation = SignDomain.t = struct
  type annotation = SignDomain.t

  let included prev state = true

  let assume _ state = (state, state)

  let merge s1 s2 = s1

  let forget var state = state

  let funcall loc lvs _ _ state = state

  let syscall loc lvs _ _ state = state

  let assign loc lv _ _ _ state = state

  let opn loc lvs _ _ _ state = state
end
