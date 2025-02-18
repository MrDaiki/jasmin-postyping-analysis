open Cmdliner
open Jasmin
open Archdef

let parse_prog filepath : ('info, 'asm) Prog.prog =
    try
      filepath
      |> Pretyping.tt_file Arch.arch_info Pretyping.Env.empty None None
      |> fst |> Pretyping.Env.decls
      |> Compile.preprocess Arch.reg_size Arch.asmOp
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
{ strict: bool
; prog: ('info, 'asm) Prog.prog }

type ('info, 'asm) command_configuration = InitVar of ('info, 'asm) init_var_arg

let run configuration =
    match configuration with
    | InitVar {strict; prog} ->
        let err = Initvars.Check.iv_prog prog strict in
        List.iter
          (fun (loc, e) -> Format.eprintf "%a: %a@." Location.pp_loc loc Initvars.Error.pp_uderror e)
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
      Arg.(value & flag & doc ["strict"])

  let term run =
      let combine prog strict = run (InitVar {strict; prog}) in
      Term.(const combine $ file_term $ strict_term)

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
