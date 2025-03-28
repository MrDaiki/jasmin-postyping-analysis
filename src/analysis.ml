open Cmdliner
open Liveness.LivenessAnalyser
open Jasmin
open ArchDef
open DeadCode.DeadCodeVisitor

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

let pinfo_term analysis_name =
    let doc =
        Arg.info ~docv:"info"
          ~doc:
            (Format.asprintf "If set, will print annoted program with %s analysis result."
               analysis_name )
    in
    Arg.(value & flag & doc ["info"])

let report_bug_string = "Report bugs to <https://github.com/MrDaiki/jasmin-postyping-analysis>"

type ('info, 'asm) init_var_arg =
{ strict: InitVars.Checker.check_mode
; pinfo: bool
; prog: ('info, 'asm) Prog.prog }

type ('info, 'asm) dead_code_arg =
{ prog: ('info, 'asm) Prog.prog
; pinfo: bool }

type ('info, 'asm) command_configuration =
| InitVar of ('info, 'asm) init_var_arg
| DeadCode of ('info, 'asm) dead_code_arg

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
    | DeadCode {pinfo; prog} ->
        let prog, err = dc_prog prog in
        if pinfo then
          Printer.pp_iprog ~debug:false LivenessDomain.pp_annot Arch.reg_size Arch.asmOp
            Format.std_formatter prog ;
        List.iter
          (fun (module Err : Error.CompileError.CompileError) ->
            Format.eprintf "%a: %a@." Location.pp_loc Err.location Err.to_text () )
          err

let filepath_arg =
    let doc = "Path of the input jasmin file" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let file_term = Term.(const parse_prog $ filepath_arg)

module DeadCodeCli = struct
  let name = "deadcode"

  let doc = "Execute liveness analysis and show all dead variable assigned in the program."

  let man = [`S Manpage.s_bugs; `P report_bug_string]

  let pinfo_term = pinfo_term "liveness"

  let term run =
      let combine prog pinfo = run (DeadCode {prog; pinfo}) in
      Term.(const combine $ file_term $ pinfo_term)

  let cmd run =
      let doc = Cmd.info name ~doc ~man in
      Cmd.v doc (term run)
end

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

  let pinfo_term = pinfo_term "reaching definition"

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

  let commands run = [InitVarCli.cmd run; DeadCodeCli.cmd run]

  let run () = Cmd.group info (commands run) |> Cmd.eval |> exit
end

let () = MainCLI.run ()
