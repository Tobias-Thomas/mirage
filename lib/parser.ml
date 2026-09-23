open Lexer
open Types

type parse_state = { lexbuf : Lexing.lexbuf; mutable current : token }

let make_parser lexbuf = { lexbuf; current = next_token lexbuf }
let peek state = state.current
let advance state = state.current <- next_token state.lexbuf

let peek_advance state =
  let s = peek state in
  advance state;
  s

let expect state token =
  if peek_advance state <> token then failwith "wrong token"

let rec parse_program state =
  match peek state with
  | EOF -> []
  | _ ->
      let stmt = parse_stmt state in
      expect state SEMICOLON;
      stmt :: parse_program state

and parse_stmt state =
  match peek_advance state with
  | OBSERVE ->
      let expr = parse_expr state in
      Observe expr
  | SAMPLE ->
      let name = parse_string state in
      Sample name
  | IDENT var -> (
      match peek_advance state with
      | DET ->
          let rhs = parse_expr state in
          LetDet (var, rhs)
      | DIST -> (
          let rhs = parse_expr state in
          match rhs with
          | Dist _ -> LetRand (var, rhs)
          | _ -> failwith "expected a dist on the rhs of ~")
      | _ -> failwith "expected = or ~ but got other token")
  | _ -> failwith "wrong token"

and parse_expr state =
  let lhs = parse_addition state in
  match peek state with
  | GT ->
      advance state;
      let rhs = parse_addition state in
      BinOp (Gt, lhs, rhs)
  | LT ->
      advance state;
      let rhs = parse_addition state in
      BinOp (Lt, lhs, rhs)
  | EQ ->
    advance state;
    let rhs = parse_addition state in
    BinOp(Eq, lhs, rhs)
  | _ -> lhs

and parse_addition state =
  let lhs = parse_multiply state in
  match peek state with
  | ADD ->
      advance state;
      let rhs = parse_multiply state in
      BinOp (Add, lhs, rhs)
  | SUB ->
      advance state;
      let rhs = parse_multiply state in
      BinOp (Sub, lhs, rhs)
  | _ -> lhs

and parse_multiply state =
  let lhs = parse_atom state in
  match peek state with
  | MUL ->
      advance state;
      let rhs = parse_atom state in
      BinOp (Mul, lhs, rhs)
  | DIV ->
      advance state;
      let rhs = parse_atom state in
      BinOp (Div, lhs, rhs)
  | _ -> lhs

and parse_atom state =
  match peek_advance state with
  | FLOAT f -> Literal (Continuous f)
  | INT i -> Literal (Discrete i)
  | BOOL b -> Literal (Binary b)
  | IDENT s -> Var s
  | UPPER_IDENT d ->
      let expr_list = parse_expr_list state in
      Dist (d, expr_list)
  | LPAREN ->
      let expr = parse_expr state in
      expect state RPAREN;
      expr
  | _ -> failwith "wrong token"

and parse_expr_list state =
  expect state LPAREN;
  parse_expr_list_inner state

and parse_expr_list_inner state =
  match peek state with
  | RPAREN ->
      advance state;
      []
  | _ -> (
      let expr = parse_expr state in
      match peek_advance state with
      | COMMA -> expr :: parse_expr_list_inner state
      | RPAREN -> [ expr ]
      | _ -> failwith "expected , or ) but got other token")

and parse_string state =
  match peek_advance state with
  | IDENT s -> s
  | _ -> failwith "expected string but got other token"
