open Jasmin
open Prog
open Sign
open Analyser.Annotation

module SignDomain = struct
  type t = signess Mv.t

  let empty func =
      let mv =
          List.fold_left
            (fun env var ->
              match var.v_ty with
              | Bty Int -> Mv.add var Integer env
              | _ -> env )
            Mv.empty func.f_args
      in
      Some (List.fold_left (fun env var -> Mv.add var Undefined env) mv (Sv.to_list (locals func)))

  let erase (x : var) (s : signess) (env : t) : t Analyser.Annotation.annotation =
      match s with
      | Undefined -> Empty
      | _ -> Annotation (Mv.modify_def Integer x (fun _ -> s) env)

  let reduce (x : var) (s : signess) (env : t) : t Analyser.Annotation.annotation =
      match s with
      | Undefined -> Empty
      | _ -> Annotation (Mv.modify_def Integer x (fun sign -> s && sign) env)

  let get (x : var) (env : t) : signess = Mv.find_default Integer x env

  let intersect (env1 : t) (env2 : t) =
      Mv.merge
        (fun _ (s1 : signess option) (s2 : signess option) ->
          match (s1, s2) with
          | Some s1, Some s2 -> Some (s1 && s2)
          | _, None
           |None, _ ->
              None )
        env1 env2

  let merge (env1 : t) (env2 : t) : t =
      Mv.merge
        (fun _ s1 s2 ->
          match (s1, s2) with
          | Some s1, Some s2 -> Some (s1 || s2)
          | s, None
           |None, s ->
              s )
        env1 env2

  let included (x : t) (y : t) =
      Mv.for_all
        (fun x s1 ->
          match Mv.find_opt x y with
          | None -> false
          | Some s2 -> sign_included s1 s2 )
        x

  let remove x (t : t) : t = Mv.remove x t

  let pp fmt ((_, env) : L.i_loc * t) : unit =
      let item_list = Mv.bindings env in
      Format.fprintf fmt "%a\n"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (x, s) -> Format.fprintf fmt "%s: %a" x.v_name pp_signess s) )
        item_list
end

let sign_integer (z : Z.t) : signess =
    match z with
    | z when z = Z.of_int 0 -> Zero
    | z when z > Z.of_int 0 -> StrictPositive
    | _ -> StrictNegative

let rec sign_expression (e : expr) (registry : SignDomain.t) : signess =
    match e with
    | Pconst z -> sign_integer z
    | Pbool _ -> Undefined
    | Parr_init _ -> Undefined
    | Pvar var -> SignDomain.get (L.unloc var.gv) registry
    | Pget (_, _, _, _, _) -> Integer
    | Psub (_, _, _, _, _) -> Undefined
    | Pload (_, _, _, _) -> Integer
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
