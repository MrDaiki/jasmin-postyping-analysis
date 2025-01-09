type signess =
| Undefined
| StrictPositive
| StrictNegative
| Zero
| Negative
| Positive
| NonZero
| Integer

let extend_zeros s =
    match s with
    | StrictPositive -> Positive
    | StrictNegative -> Negative
    | NonZero -> Integer
    | a -> a

let neg s =
    match s with
    | Undefined -> Undefined
    | StrictPositive -> Negative
    | StrictNegative -> Positive
    | Zero -> Zero
    | Negative -> StrictPositive
    | Positive -> StrictNegative
    | NonZero -> NonZero
    | Integer -> Integer

let inv s =
    match s with
    | Zero -> Undefined
    | s -> s

let ( || ) s1 s2 =
    match (s1, s2) with
    | Undefined, a
     |a, Undefined ->
        a (* uncorrect *)
    | Integer, _
     |_, Integer ->
        Integer
    | Zero, a
     |a, Zero ->
        extend_zeros a
    | Negative, Negative -> Negative
    | Positive, Positive -> Positive
    | StrictNegative, StrictNegative -> StrictNegative
    | StrictPositive, StrictPositive -> StrictPositive
    | NonZero, NonZero -> NonZero
    | Negative, (Positive | NonZero | StrictPositive)
     |(Positive | NonZero | StrictPositive), Negative ->
        Integer
    | Positive, (NonZero | StrictNegative)
     |(NonZero | StrictNegative), Positive ->
        Integer
    | Negative, StrictNegative
     |StrictNegative, Negative ->
        Negative
    | Positive, StrictPositive
     |StrictPositive, Positive ->
        Positive
    | NonZero, (StrictNegative | StrictPositive)
     |(StrictNegative | StrictPositive), NonZero ->
        NonZero
    | StrictNegative, StrictPositive
     |StrictPositive, StrictNegative ->
        NonZero

let ( + ) s1 s2 =
    match (s1, s2) with
    | Undefined, _
     |_, Undefined ->
        Undefined (* uncorrect *)
    | StrictPositive, StrictPositive -> StrictPositive
    | StrictNegative, StrictNegative -> StrictNegative
    | Zero, a
     |a, Zero ->
        a
    | Negative, Negative -> Negative
    | Positive, Positive -> Positive
    | Negative, StrictNegative
     |StrictNegative, Negative ->
        StrictNegative
    | Positive, StrictPositive
     |StrictPositive, Positive ->
        StrictPositive
    | _ -> Integer

let ( - ) s1 s2 = s1 + neg s2

let ( * ) s1 s2 =
    match (s1, s2) with
    | Undefined, _
     |_, Undefined ->
        Undefined (* uncorrect *)
    | Zero, _
     |_, Zero ->
        Zero
    | StrictPositive, a
     |a, StrictPositive ->
        a
    | StrictNegative, a
     |a, StrictNegative ->
        neg a
    | Positive, a
     |a, Positive ->
        extend_zeros a
    | Negative, a
     |a, Negative ->
        extend_zeros (neg a)
    | NonZero, a
     |a, NonZero ->
        a
    | Integer, _ -> Integer

let ( / ) s1 s2 = s1 * inv s2
