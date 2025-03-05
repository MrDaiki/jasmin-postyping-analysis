open Jasmin
open Prog

type memory_effect =
| None
| Some
| Depends of Sf.t

let ( || ) e1 e2 =
    match (e1, e2) with
    | a, None
     |None, a ->
        a
    | Some, _
     |_, Some ->
        Some
    | Depends fns1, Depends fns2 -> Depends (Sf.union fns1 fns2)
