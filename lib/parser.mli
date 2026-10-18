type parse_state

val make_parser : Lexing.lexbuf -> parse_state
val parse_program : parse_state -> Types.program
