open Jasmin
open Prog
open Utils
open ReturnEffect

let rt_prog (_, funcs) : return_effect Mf.t =
    List.fold
      (fun acc f ->
        let effect =
            List.fold
              (fun eff v ->
                match (L.unloc v).v_kind with
                | Reg (_, Pointer _)
                 |Stack (Pointer _) ->
                    eff
                | _ -> Some )
              ReturnEffect.None f.f_ret
        in
        Mf.add f.f_name effect acc )
      Mf.empty funcs
