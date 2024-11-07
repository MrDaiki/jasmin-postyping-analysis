open Jasmin

(* open Rd_structured *)
open Staticvars.Mutator
open Mutability.Mutator
open Initvars.Check

let check prog = iv_prog prog ; md_prog prog ; ud_prog prog

module Arch =
  ( val let use_set0 = true and use_lea = false in
        let call_conv = Glob_options.Linux in
        let module C : Arch_full.Core_arch =
          (val CoreArchFactory.core_arch_x86 ~use_lea ~use_set0 call_conv)
        in
        (module Arch_full.Arch_from_Core_arch (C) : Arch_full.Arch) )

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
