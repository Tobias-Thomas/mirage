type state

val init : unit -> state

type command =
  | Load of string
  | Sample of int
  | Seed of int
  | Observe of string * Types.value
  | Unobserve of string

val parse_command : string -> (command, string) result
val execute : state -> command -> (state * string, string) result
