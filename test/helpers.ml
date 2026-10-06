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
      | { stmts = [ Types.LetDet ("x", e) ]; observations = [] } ->
          Types.show_expr e
      | _ -> failwith "unexpected program shape")

(* Parses [s] as the right-hand side of a random assignment and prints just the
   distribution. *)
let print_dist s =
  print_or_error (fun () ->
      match parse ("x ~ " ^ s ^ ";") with
      | { stmts = [ Types.LetRand ("x", d) ]; observations = [] } ->
          Types.show_dist d
      | _ -> failwith "unexpected program shape")

let show_error (e : Eval.error) =
  match e with
  | Unbound_variable msg -> "Unbound_variable: " ^ msg
  | Redefined_variable msg -> "Redefined_variable: " ^ msg
  | Type_error msg -> "Type_error: " ^ msg
  | Invalid_distribution msg -> "Invalid_distribution: " ^ msg

(* Parses and evaluates [s] with a fixed seed, then prints every variable
   (sorted by name) and the log weight, or the runtime error. *)
let print_eval ?(seed = 42) s =
  print_or_error (fun () ->
      let rng = Random.State.make [| seed |] in
      match Eval.eval_program rng (parse s) with
      | Ok { env; log_weight } ->
          let bindings =
            List.map
              (fun (var, v) -> var ^ " = " ^ Types.show_value v)
              (Eval.Env.bindings env)
          in
          String.concat "\n"
            (bindings @ [ Printf.sprintf "log_weight = %g" log_weight ])
      | Error e -> "runtime error: " ^ show_error e)

(* Evaluates [s] once per seed and returns the value of [var] in each run. *)
let sample_var ~seeds var s =
  let prog = parse s in
  List.map
    (fun seed ->
      let rng = Random.State.make [| seed |] in
      match Eval.eval_program rng prog with
      | Ok { env; _ } -> Eval.Env.find var env
      | Error e -> failwith (show_error e))
    seeds
