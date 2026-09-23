{
  type token =
    | FLOAT of float
    | INT of int
    | BOOL of bool
    | IDENT of string
    | UPPER_IDENT of string
    | OBSERVE | SAMPLE
    | ADD | SUB | MUL | DIV
    | GT | LT | EQ | DET | DIST
    | LPAREN | RPAREN | COMMA | SEMICOLON
    | EOF
  [@@deriving show { with_path = false }]
}

rule next_token = parse
| [' ' '\t' '\n'] { next_token lexbuf }
| ';' { SEMICOLON }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { ADD }
| '-' { SUB }
| '*' { MUL }
| '/' { DIV }
| '>' { GT }
| '<' { LT }
| '~' { DIST }
| "==" { EQ }
| '=' { DET }
| ['0'-'9']+ '.' ['0'-'9']* as f { FLOAT (float_of_string f) }
| ['0'-'9']+ as n { INT (int_of_string n) }
| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as s {
  match s with
  | "observe" -> OBSERVE
  | "sample" -> SAMPLE
  | "true" -> BOOL true
  | "false" -> BOOL false
  | _ -> IDENT s
}
| ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as s { UPPER_IDENT s }
| eof { EOF }
| _ as c { failwith (Printf.sprintf "Unexpected character: %c" c) }
