open Jasmin
open Signess.Analyser
open Prog

module Arch =
  ( val let use_set0 = true and use_lea = false in
        let call_conv = Glob_options.Linux in
        let module C : Arch_full.Core_arch =
          (val CoreArchFactory.core_arch_x86 ~use_lea ~use_set0 call_conv)
        in
        (module Arch_full.Arch_from_Core_arch (C) : Arch_full.Arch) )

let check ((gbs, funcs) : ('info, 'asm) prog) =
    let annotated_funcs = sg_prog (gbs, funcs) in
    let funcs = List.map (fun (f, _) -> f) annotated_funcs in
    Printer.pp_iprog ~debug:true SignAnalyserLogic.pp_annot Arch.reg_size Arch.asmOp
      Format.std_formatter (gbs, funcs) ;
    ()

let () =
    try
      Sys.argv.(1)
      |> Pretyping.tt_file Arch.arch_info Pretyping.Env.empty None None
      |> fst |> Pretyping.Env.decls
      |> Compile.preprocess Arch.reg_size Arch.asmOp
      |> check |> ignore
    with
        | Pretyping.TyError (loc, e) ->
            Format.eprintf "%a: %a@." Location.pp_loc loc Pretyping.pp_tyerror e ;
            exit 2
        | Syntax.ParseError (loc, None) ->
            Format.eprintf "Parse error: %a@." Location.pp_loc loc ;
            exit 3
        | Invalid_argument _ ->
            Format.eprintf "usage: %s FILE" Sys.argv.(0) ;
            exit 4
        | Staticvars.Error.IVError (loc, e) ->
            Format.eprintf "%a: %a@." Location.pp_loc loc Staticvars.Error.pp_iverror e ;
            exit 5
        | Mutability.Error.MdError (loc, e) ->
            Format.eprintf "%a: %a@." Location.pp_loc loc Mutability.Error.pp_mderror e ;
            exit 6
        | Initvars.Error.UvError (loc, e) ->
            Format.eprintf "%a: %a@." Location.pp_loc loc Initvars.Error.pp_uderror e ;
            exit 7
