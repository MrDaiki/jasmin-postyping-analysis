open Jasmin.Prog
open Memory.Memoryeffect
open Memory.Check
open Mutate_args.Check
open Mutate_args.Mutparameffect
open Return_values.Check
open Return_values.Return_effect

let check_effects
    (rt_effect : return_effect)
    (mp_effect : mut_param_effect)
    (mm_effect : memory_effect) : bool =
    match (rt_effect, mp_effect, mm_effect) with
    | None, None, None -> false
    | _ -> true

let ef_check (p : ('info, 'asm) prog) =
    let return_effects = rt_prog p in
    let memory_effects = mem_prog p in
    let mut_param_effects = mp_prog p in
    Mf.iter
      (fun k mut_param_effect ->
        let return_effect = Mf.find k return_effects in
        let memory_effects = Mf.find k memory_effects in
        if not (check_effects return_effect mut_param_effect memory_effects) then
          Printf.printf "Warning : Function %s has no effect\n" k.fn_name )
        (* TODO : handle warning formating properly*)
      mut_param_effects
