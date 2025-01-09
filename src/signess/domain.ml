open Jasmin
open Prog
open Sign

module SignDomain = struct
  type t = signess Mv.t

  let update (x : var) (s : signess) (env : t) : t = Mv.add x s env

  let get (x : var) (env : t) : signess = Mv.find x env

  let merge (env1 : t) (env2 : t) : t =
      Mv.merge
        (fun _ s1 s2 ->
          match (s1, s2) with
          | Some s1, Some s2 -> Some (s1 || s2)
          | s, None
           |None, s ->
              s )
        env1 env2
end

let sign_integer (z : Z.t) : signess =
    match z with
    | z when z = Z.of_int 0 -> Zero
    | z when z > Z.of_int 0 -> StrictPositive
    | _ -> StrictNegative

let rec sign_expression (e : expr) registry : signess =
    match e with
    | Pconst z -> sign_integer z
    | Pbool _ -> Undefined
    | Parr_init _ -> Undefined
    | Pvar var -> SignDomain.get (L.unloc var.gv) registry
    | Pget (_, _, _, _, _) -> Undefined
    | Psub (_, _, _, _, _) -> Undefined
    | Pload (_, _, _, _) -> Undefined
    | Papp1 (op, expr) -> (
        match op with
        | Oneg _ -> neg (sign_expression expr registry)
        | _ -> Undefined )
    | Papp2 (op, l, r) -> (
        match op with
        | Oadd _ -> sign_expression l registry + sign_expression r registry
        | Osub _ -> sign_expression l registry - sign_expression r registry
        | Omul _ -> sign_expression l registry * sign_expression r registry
        | Odiv _ -> sign_expression l registry / sign_expression r registry
        | _ -> Undefined )
    | PappN (_, _) -> Undefined
    | Pif (_, e1, _, _) -> sign_expression e1 registry
