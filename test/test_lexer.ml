open Helpers

let%expect_test "empty input" =
  print_tokens "";
  [%expect {||}]

let%expect_test "whitespace only" =
  print_tokens " \t\n  ";
  [%expect {||}]

let%expect_test "punctuation" =
  print_tokens "( ) , ;";
  [%expect {| LPAREN RPAREN COMMA SEMICOLON |}]

let%expect_test "arithmetic operators" =
  print_tokens "+ - * /";
  [%expect {| ADD SUB MUL DIV |}]

let%expect_test "comparison operators" =
  print_tokens "< > ==";
  [%expect {| LT GT EQ |}]

let%expect_test "= vs == (longest match)" =
  print_tokens "x = y";
  print_tokens "x==y";
  print_tokens "===";
  [%expect {|
    (IDENT "x") DET (IDENT "y")
    (IDENT "x") EQ (IDENT "y")
    EQ DET
    |}]

let%expect_test "~ is DIST" =
  print_tokens "x ~ y";
  [%expect {| (IDENT "x") DIST (IDENT "y") |}]

let%expect_test "numbers" =
  print_tokens "42 1.5 2.";
  [%expect {| (INT 42) (FLOAT 1.5) (FLOAT 2.) |}]

let%expect_test "keywords" =
  print_tokens "observe sample true false";
  [%expect {| OBSERVE SAMPLE (BOOL true) (BOOL false) |}]

let%expect_test "keyword prefix is an identifier" =
  print_tokens "observer samples";
  [%expect {| (IDENT "observer") (IDENT "samples") |}]

let%expect_test "identifiers" =
  print_tokens "mu_1 sigmaSq Normal";
  [%expect {| (IDENT "mu_1") (IDENT "sigmaSq") (UPPER_IDENT "Normal") |}]

let%expect_test "distribution call" =
  print_tokens "x ~ Uniform(0.0, 1);";
  [%expect {| (IDENT "x") DIST (UPPER_IDENT "Uniform") LPAREN (FLOAT 0.) COMMA (INT 1) RPAREN SEMICOLON |}]

let%expect_test "unexpected character" =
  print_tokens "$";
  [%expect {| error: Failure("Unexpected character: $") |}]

let%expect_test "newlines advance the line counter" =
  let lexbuf = Lexing.from_string "a\nb" in
  ignore (Mirage.Lexer.next_token lexbuf);
  ignore (Mirage.Lexer.next_token lexbuf);
  Printf.printf "line %d\n" Lexing.(lexbuf.lex_curr_p.pos_lnum);
  [%expect {| line 2 |}]
