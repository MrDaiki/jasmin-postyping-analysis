open Error.CompileError

module DeadCodeError (P : sig
  val payload : Jasmin.Prog.var

  val loc : Jasmin.Location.t
end) =
struct
  type payload = Jasmin.Prog.var

  let payload = P.payload

  let location = P.loc

  let error_strategy = Error.Recover.AlwaysWarn

  let code = "DC-E001"

  let to_serial fmt () = Format.fprintf fmt "{\"error\":%s}" code

  let to_text fmt () =
      Format.fprintf fmt "Variable '%a' is assigned but never used"
        (Jasmin.Printer.pp_var ~debug:false)
        payload
end

let create_dead_code_error (err_payload : Jasmin.Prog.var) (loc : Jasmin.Location.t) : compile_error
    =
    let module M =
      DeadCodeError (struct
        let payload = err_payload

        let loc = loc
      end)
    in
    (module M : CompileError)
