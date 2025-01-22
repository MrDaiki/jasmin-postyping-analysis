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

let not = function
    | Undefined -> Integer
    | StrictPositive -> Negative
    | StrictNegative -> Positive
    | Zero -> NonZero
    | Negative -> StrictPositive
    | Positive -> StrictNegative
    | NonZero -> Zero
    | Integer -> Undefined

let neg s =
    match s with
    | Undefined -> Undefined
    | StrictPositive -> StrictNegative
    | StrictNegative -> StrictPositive
    | Zero -> Zero
    | Negative -> Positive
    | Positive -> Negative
    | NonZero -> NonZero
    | Integer -> Integer

let inv s =
    match s with
    | Zero -> Undefined
    | s -> s

let sign_included s1 s2 =
    match (s1, s2) with
    | Undefined, _ -> true
    | _, Undefined -> false
    | _, Integer -> true
    | Integer, (Zero | NonZero | StrictNegative | Negative | StrictPositive | Positive) -> false
    | Zero, Zero -> true
    | NonZero, NonZero -> true
    | StrictNegative, StrictNegative -> true
    | StrictNegative, (StrictPositive | Zero | Positive) -> false
    | StrictPositive, StrictPositive -> true
    | StrictPositive, (StrictNegative | Zero | Negative) -> false
    | (StrictNegative | Zero | Negative), Negative -> true
    | (StrictPositive | Zero | Positive), Positive -> true
    | (StrictNegative | StrictPositive), NonZero -> true
    | Zero, (StrictNegative | StrictPositive | NonZero) -> false
    | Negative, (StrictPositive | Positive | NonZero | StrictNegative | Zero) -> false
    | Positive, (StrictNegative | Negative | NonZero | StrictPositive | Zero) -> false
    | NonZero, (StrictNegative | Negative | StrictPositive | Positive | Zero) -> false

let ( && ) s1 s2 =
    match (s1, s2) with
    | Undefined, _
     |_, Undefined ->
        Undefined
    | Integer, a
     |a, Integer ->
        a
    | Zero, (StrictNegative | NonZero | StrictPositive)
     |(StrictNegative | NonZero | StrictPositive), Zero ->
        Undefined
    | Zero, (Negative | Positive | Zero)
     |(Negative | Positive), Zero ->
        Zero
    | StrictNegative, (StrictPositive | Positive)
     |(StrictPositive | Positive), StrictNegative ->
        Undefined
    | StrictNegative, (StrictNegative | Negative | NonZero)
     |(Negative | NonZero), StrictNegative ->
        StrictNegative
    | StrictPositive, Negative
     |Negative, StrictPositive ->
        Undefined
    | StrictPositive, (StrictPositive | NonZero | Positive)
     |(NonZero | Positive), StrictPositive ->
        StrictPositive
    | Negative, Positive
     |Positive, Negative ->
        Zero
    | NonZero, Negative
     |Negative, NonZero ->
        StrictNegative
    | NonZero, Positive
     |Positive, NonZero ->
        StrictPositive
    | NonZero, NonZero -> NonZero
    | Negative, Negative -> Negative
    | Positive, Positive -> Positive

let ( || ) s1 s2 =
    match (s1, s2) with
    | Undefined, a
     |a, Undefined ->
        a
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

let pp_signess fmt = function
    | Undefined -> Format.fprintf fmt "Undefined"
    | StrictPositive -> Format.fprintf fmt "StrictPositive"
    | StrictNegative -> Format.fprintf fmt "StrictNegative"
    | Zero -> Format.fprintf fmt "Zero"
    | Negative -> Format.fprintf fmt "Negative"
    | Positive -> Format.fprintf fmt "Positive"
    | NonZero -> Format.fprintf fmt "NonZero"
    | Integer -> Format.fprintf fmt "Integer"
