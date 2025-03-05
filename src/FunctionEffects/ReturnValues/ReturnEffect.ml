type return_effect =
| None
| Some

let ( || ) e1 e2 =
    match (e1, e2) with
    | None, None -> None
    | Some, _
     |_, Some ->
        Some

let string_of_return_effect = function
    | None -> "None"
    | Some -> "Some"
