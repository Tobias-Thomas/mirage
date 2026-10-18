open Types

type command =
  | Load of string
  | Sample of int
  | Seed of int
  | Observe of string * value
  | Unobserve of string

let parse_command s =
  match List.filter (fun w -> w <> "") (String.split_on_char ' ' s) with
  | [ "load"; path ] -> Ok (Load path)
  | "load" :: _ -> Error "Load requires exactly one argument"
  | [ "unobserve"; variable ] -> Ok (Unobserve variable)
  | "unobserve" :: _ -> Error "Unobserve requires exactly one argument"
  | "sample" :: args -> (
      match List.map int_of_string_opt args with
      | [ Some n ] when n > 0 -> Ok (Sample n)
      | _ -> Error "Sample requires exactly one positive integer argument")
  | "seed" :: args -> (
      match List.map int_of_string_opt args with
      | [ Some n ] -> Ok (Seed n)
      | _ -> Error "Seed requires exactly one integer argument")
  | [ "observe"; variable; value ] -> (
      let lex_buf = Lexing.from_string value in
      let p_state = Parser.make_parser lex_buf in
      let val_parsed_opt =
        Parser.value_from_expr_opt (Parser.parse_atom p_state)
      in
      match val_parsed_opt with
      | Some v -> Ok (Observe (variable, v))
      | None -> Error (Printf.sprintf "%s must parse as a concrete value" value)
      )
  | "observe" :: _ -> Error "Observe requires exactly two arguments"
  | command :: _ -> Error (Printf.sprintf "%s is not a known command" command)
  | [] -> Error "The command should not be empty"
