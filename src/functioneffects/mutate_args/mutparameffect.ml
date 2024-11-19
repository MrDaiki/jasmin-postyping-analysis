open Jasmin
open Prog

type mut_param_effect =
| None
| Some
| Depends of Sv.t

let ( || ) e1 e2 =
    match (e1, e2) with
    | a, None
     |None, a ->
        a
    | Some, _
     |_, Some ->
        Some
    | Depends fns1, Depends fns2 -> Depends (Sv.union fns1 fns2)
