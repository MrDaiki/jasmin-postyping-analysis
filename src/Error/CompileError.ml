open Recover

module type CompileError = sig
  type payload

  val payload : payload

  val location : Jasmin.Location.t

  val error_strategy : recover_flags

  val code : string

  val to_serial : Format.formatter -> unit -> unit

  val to_text : Format.formatter -> unit -> unit
end

type compile_error = (module CompileError)
