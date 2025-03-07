open Cmdliner
open Jasmin
open ArchDef

let parse_prog filepath : ('info, 'asm) Prog.prog =
    try
      let env, _, _ = Compile.parse_file Arch.arch_info filepath in
      env |> Pretyping.Env.decls |> Compile.preprocess Arch.reg_size Arch.asmOp
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

let report_bug_string = "Report bugs to <https://github.com/MrDaiki/jasmin-postyping-analysis>"

type ('info, 'asm) init_var_arg =
{ strict: InitVars.Checker.check_mode
; pinfo: bool
; prog: ('info, 'asm) Prog.prog }

type ('info, 'asm) command_configuration = InitVar of ('info, 'asm) init_var_arg

let run configuration =
    match configuration with
    | InitVar {strict; pinfo; prog} ->
        let prog, err = InitVars.Check.iv_prog prog strict in
        if pinfo then
          Printer.pp_iprog ~debug:false Rd.RdAnalyser.ReachingDefinitionLogic.pp_annot Arch.reg_size
            Arch.asmOp Format.std_formatter prog ;
        List.iter
          (fun (module Err : Error.CompileError.CompileError) ->
            Format.eprintf "%a: %a@." Location.pp_loc Err.location Err.to_text () )
          err

let filepath_arg =
    let doc = "Path of the input jasmin file" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let file_term = Term.(const parse_prog $ filepath_arg)

module InitVarCli = struct
  let name = "initvars"

  let doc =
      "Execute reaching definition analysis and show all unitialised variable of the input program."

  let man = [`S Manpage.s_bugs; `P report_bug_string]

  let strict_term =
      let doc =
          Arg.info ~docv:"strict"
            ~doc:
              "If set, analyse will raise an error if there is at least one path in execution \
               where variable may not be initialised. If not set, error will be raised only if \
               there is no path where variable is initialised."
      in
      let flag_to_mode = function
          | true -> InitVars.Checker.Strict
          | false -> InitVars.Checker.NotStrict
      in
      Term.(const flag_to_mode $ Arg.(value & flag & doc ["strict"]))

  let pinfo_term =
      let doc =
          Arg.info ~docv:"info"
            ~doc:"If set, will print annoted program with reaching definition analysis result."
      in
      Arg.(value & flag & doc ["info"])

  let term run =
      let combine prog strict pinfo = run (InitVar {strict; pinfo; prog}) in
      Term.(const combine $ file_term $ strict_term $ pinfo_term)

  let cmd run =
      let doc = Cmd.info name ~doc ~man in
      Cmd.v doc (term run)
end

module MainCLI = struct
  let name = "jasmin-linter"

  let doc =
      "Jasmin linting tool. Implement several type hinting and program analysis for jasmin \
       programing language."

  let man = [`S Manpage.s_description; `P doc]

  let info = Cmd.info name ~doc ~man

  let commands run = [InitVarCli.cmd run]

  let run () = Cmd.group info (commands run) |> Cmd.eval |> exit
end

let () = MainCLI.run ()
