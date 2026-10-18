type parse_state

val make_parser : Lexing.lexbuf -> parse_state
val parse_program : parse_state -> Types.program
val parse_atom : parse_state -> Types.expr
val value_from_expr_opt : Types.expr -> Types.value option
