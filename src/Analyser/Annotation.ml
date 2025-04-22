type 'domain annotation =
| Empty
| Annotation of 'domain

let pp_annotation pp_domain fmt (loc, annot) =
    match annot with
    | Empty -> Format.fprintf fmt "Empty"
    | Annotation domain -> pp_domain fmt (loc, domain)

let merge_annotation d1 d2 merge =
    match (d1, d2) with
    | Empty, a
     |a, Empty ->
        a
    | Annotation d1, Annotation d2 -> Annotation (merge d1 d2)

let included_annotation a b included =
    match (a, b) with
    | Empty, _ -> true
    | _, Empty -> false
    | Annotation a, Annotation b -> included a b

let bind_annotation b f =
    match b with
    | Empty -> Empty
    | Annotation a -> f a

let unwrap_annotation annotation =
    match annotation with
    | Empty -> assert false (* The provided domain can't be empty if you use this function*)
    | Annotation a -> a
