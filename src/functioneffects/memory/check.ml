open Jasmin
open Prog
open Memoryeffect
open Mutator

let is_reduced map =
    Mf.for_all
      (fun _ effect ->
        match effect with
        | None -> true
        | Some -> true
        | Depends _ -> false )
      map

let reduce_dependency (deps : Sf.t) (effects : memory_effect Mf.t) : memory_effect =
    Sf.fold
      (fun dep effect ->
        let dep_effect = Mf.find dep effects in
        match dep_effect with
        | None -> effect
        | Some -> Some
        | Depends _ -> Depends (Sf.singleton dep) || effect )
      deps None

let rec reduce_memory_effect (effects : memory_effect Mf.t) : memory_effect Mf.t =
    let new_map =
        Mf.map
          (fun effect ->
            match effect with
            | None -> None
            | Some -> Some
            | Depends fns -> reduce_dependency fns effects )
          effects
    in
    if is_reduced new_map then
      new_map
    else
      reduce_memory_effect new_map

let mem_prog (prog : ('info, 'asm) prog) =
    let memory_effects = memory_effects prog in
    reduce_memory_effect memory_effects
