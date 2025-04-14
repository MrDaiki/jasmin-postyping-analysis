type 'domain annotation =
| Empty
| Annotation of 'domain

let merge_annotations (a1 : 'domain annotation) (a2 : 'domain annotation) merge_domain =
    match (a1, a2) with
    | Empty, _ -> a2
    | _, Empty -> a1
    | Annotation d1, Annotation d2 -> Annotation (merge_domain d1 d2)

let pp_annotation
    (pp_domain : Format.formatter -> Jasmin.Location.i_loc * 'domain -> unit)
    fmt
    ((loc, annot) : Jasmin.Location.i_loc * 'domain annotation) =
    match annot with
    | Empty -> Format.fprintf fmt "{}"
    | Annotation d -> pp_domain fmt (loc, d)

let forward (func : 'domain -> 'domain annotation) (annotation : 'domain annotation) =
    match annotation with
    | Empty -> Empty
    | Annotation d -> func d
