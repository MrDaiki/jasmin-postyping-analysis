open Jasmin
open Prog
open Utils
open Mutparameffect
open MutableArgsVisitor

let is_reduced (varmap : mut_param_effect Mv.t) : bool =
    Mv.for_all
      (fun _ effect ->
        match effect with
        | None -> true
        | Some -> true
        | Depends _ -> false )
      varmap

let reduce_dependencies (fns : Sv.t) (varmap : mut_param_effect Mv.t) : mut_param_effect =
    Sv.fold
      (fun fn effect ->
        Printf.printf "%s\n" fn.v_name ;
        let fn_effect = Mv.find fn varmap in
        match fn_effect with
        | None -> effect
        | Some -> Some
        | Depends _ -> effect )
      fns None

let rec reduce (varmap : mut_param_effect Mv.t) : mut_param_effect Mv.t =
    let new_map =
        Mv.map
          (fun effect ->
            match effect with
            | None -> None
            | Some -> Some
            | Depends vars -> reduce_dependencies vars varmap )
          varmap
    in
    if is_reduced new_map then
      new_map
    else
      reduce new_map

let mp_prog (prog : ('info, 'asm) prog) =
    let _, funcs = prog in
    let varmap =
        (MutableArgsVisitor.visit_prog prog MutableArgsVisitor.initial_state).mutable_params_effect
    in
    let varmap = reduce varmap in
    List.fold
      (fun acc func ->
        let mut_params =
            List.fold
              (fun acc' var ->
                if is_mutable_ptr var then
                  Sv.add var acc'
                else
                  acc' )
              Sv.empty func.f_args
        in
        let func_effect = Sv.fold (fun var acc -> acc || Mv.find var varmap) mut_params None in
        Mf.add func.f_name func_effect acc )
      Mf.empty funcs
