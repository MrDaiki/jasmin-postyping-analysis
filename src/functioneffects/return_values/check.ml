open Jasmin
open Prog
open Utils
open Return_effect

let rt_prog (_, funcs) : return_effect Mf.t =
    List.fold
      (fun acc f ->
        let effect =
            List.fold
              (fun effect v ->
                match v.v_kind with
                | Reg (_, Pointer _)
                 |Stack (Pointer _) ->
                    effect
                | _ -> Some )
              Return_effect.None f.f_args
        in
        Mf.add f.f_name effect acc )
      Mf.empty funcs
