open Mirage

(* Each printer catches exceptions and prints them instead, so error cases can
   be recorded in [%expect] blocks just like successful ones. *)
let print_or_error f =
  match f () with
  | s -> print_endline s
  | exception e -> print_endline ("error: " ^ Printexc.to_string e)

let tokens s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.next_token lexbuf with
    | Lexer.EOF -> List.rev acc
    | t -> loop (t :: acc)
  in
  loop []

let parse s = Parser.parse_program (Parser.make_parser (Lexing.from_string s))

let print_tokens s =
  print_or_error (fun () ->
      String.concat " " (List.map Lexer.show_token (tokens s)))

let print_program s = print_or_error (fun () -> Types.show_program (parse s))

(* Parses [s] as the right-hand side of a deterministic assignment and prints
   just the expression. *)
let print_expr s =
  print_or_error (fun () ->
      match parse ("x = " ^ s ^ ";") with
      | [ Types.LetDet ("x", e) ] -> Types.show_expr e
      | _ -> failwith "unexpected program shape")
