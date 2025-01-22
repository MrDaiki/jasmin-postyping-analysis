open Jasmin
open Prog
open Sign
open Domain
open Analyser
open Expr

let reduce_lt_left sign_x sign_y =
    match (sign_x, sign_y) with
    | Undefined, _
     |_, Undefined ->
        Undefined
        (* Undefined because if x or y is Undefined, then x < y has no model, thus no execution*)
    | StrictNegative, _ -> StrictNegative
    | Negative, (StrictNegative | Negative | Zero) -> StrictNegative
    | Negative, _ -> Negative
    | Zero, (StrictNegative | Negative | Zero) -> Undefined
    | Zero, _ -> Zero
    | Positive, (StrictNegative | Negative | Zero) -> Undefined
    | Positive, _ -> Positive
    | StrictPositive, (StrictNegative | Negative | Zero) -> Undefined
    | StrictPositive, _ -> StrictPositive
    | NonZero, (StrictNegative | Negative | Zero) -> StrictNegative
    | NonZero, _ -> NonZero
    | Integer, (StrictNegative | Negative | Zero) -> StrictNegative
    | Integer, _ -> Integer

let reduce_lt_right sign_x sign_y =
    match (sign_x, sign_y) with
    | Undefined, _
     |_, Undefined ->
        Undefined
    | (Positive | Zero | StrictPositive), StrictNegative -> Undefined
    | _, StrictNegative -> StrictNegative
    | (Positive | Zero | StrictNegative), Negative -> Undefined
    | _, Negative -> Negative
    | (Positive | Zero | StrictPositive), Zero -> Undefined
    | _, Zero -> Zero
    | (Positive | Zero | StrictNegative), Positive -> StrictPositive
    | _, Positive -> Positive
    | (Positive | Zero | StrictPositive), StrictPositive -> StrictPositive
    | _, StrictPositive -> StrictPositive
    | (Positive | Zero | StrictPositive), NonZero -> StrictPositive
    | _, NonZero -> NonZero
    | (Positive | Zero | StrictPositive), Integer -> StrictPositive
    | _, Integer -> Integer

let reduce_domain (e : expr) (sign : Sign.signess) (domain : SignDomain.t) : SignDomain.t =
    match e with
    | Pvar v -> (
        match (L.unloc v.gv).v_ty with
        | Bty Int -> SignDomain.reduce (L.unloc v.gv) sign domain
        | _ -> domain )
    | _ -> domain

let rec condition_analysis (e : expr) (domain : SignDomain.t) : SignDomain.t * SignDomain.t =
    match e with
    | Papp2 (Oeq Op_int, l, r) ->
        let l_sign, r_sign = (sign_expression l domain, sign_expression r domain) in
        let valid_sign : Sign.signess = l_sign && r_sign in
        let invalid_sign = not valid_sign in
        let valid_l = reduce_domain l valid_sign domain in
        let valid_r = reduce_domain r valid_sign domain in
        let invalid_l = reduce_domain l invalid_sign domain in
        let invalid_r = reduce_domain r invalid_sign domain in
        (SignDomain.intersect valid_l valid_r, SignDomain.intersect invalid_l invalid_r)
    | Papp2 (Olt Cmp_int, l, r) ->
        let l_sign, r_sign = (sign_expression l domain, sign_expression r domain) in
        let valid_l_sign = reduce_lt_left l_sign r_sign in
        let valid_r_sign = reduce_lt_right l_sign r_sign in
        let invalid_l_sign = not valid_l_sign in
        let invalid_r_sign = not valid_r_sign in
        let valid_l = reduce_domain l valid_l_sign domain in
        let valid_r = reduce_domain r valid_r_sign domain in
        let invalid_l = reduce_domain l invalid_l_sign domain in
        let invalid_r = reduce_domain r invalid_r_sign domain in
        (SignDomain.intersect valid_l valid_r, SignDomain.intersect invalid_l invalid_r)
    | Papp2 (Ole Cmp_int, l, r) ->
        let lt, lf = condition_analysis (Papp2 (Olt Cmp_int, l, r)) domain in
        let et, ef = condition_analysis (Papp2 (Oeq Op_int, l, r)) domain in
        (SignDomain.merge lt et, SignDomain.merge lf ef)
    | Papp2 (Ogt Cmp_int, l, r) -> condition_analysis (Papp2 (Olt Cmp_int, r, l)) domain
    | Papp2 (Oge Cmp_int, l, r) -> condition_analysis (Papp2 (Ole Cmp_int, r, l)) domain
    | _ -> (domain, domain)

let lv_int (lv : int glval) =
    match lv with
    | Lvar v -> (
        match (L.unloc v).v_ty with
        | Bty Int -> Some (L.unloc v)
        | _ -> None )
    | _ -> None

let lvs_int lvs = List.filter_map lv_int lvs

module SignAnalyserLogic : AnalyserLogic with type annotation = SignDomain.t = struct
  type annotation = SignDomain.t

  let included prev state = SignDomain.included prev state

  let assume cond state = condition_analysis cond state

  let merge s1 s2 = SignDomain.merge s1 s2

  let forget var state = SignDomain.remove (L.unloc var) state

  let funcall _ lvs _ _ state =
      let int_lvs = lvs_int lvs in
      let state =
          List.fold_left (fun state lv -> SignDomain.erase lv Integer state) state int_lvs
      in
      state

  let syscall _ lvs _ _ state =
      let int_lvs = lvs_int lvs in
      let state =
          List.fold_left (fun state lv -> SignDomain.erase lv Integer state) state int_lvs
      in
      state

  let assign _ lv _ _ expr state =
      let int_lvs = lvs_int [lv] in
      List.fold_left
        (fun state lv -> SignDomain.erase lv (sign_expression expr state) state)
        state int_lvs

  let opn _ lvs _ _ (exprs : exprs) state =
      let combine = List.combine lvs exprs in
      let compare_values =
          List.filter_map
            (fun (lv, expr) ->
              match lv_int lv with
              | Some lv -> Some (lv, sign_expression expr state)
              | None -> None )
            combine
      in
      List.fold_left (fun state (lv, sign) -> SignDomain.reduce lv sign state) state compare_values
end

module SignAnalyser = TreeAnalyser.Make (SignAnalyserLogic)

let sg_prog (_, funcs) =
    List.map (fun f -> SignAnalyser.analyse_function f (SignDomain.empty f)) funcs
