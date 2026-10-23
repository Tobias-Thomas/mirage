open Types
open Eval

type state = {
  program : program option;
  observation_map : value Env.t;
  rng : Random.State.t;
  last_samples : (trace list * float list) option;
}

let init () =
  {
    program = None;
    observation_map = Env.empty;
    rng = Random.State.make_self_init ();
    last_samples = None;
  }

type command =
  | Load of string
  | Sample of int
  | Seed of int
  | Observe of string * value
  | Unobserve of string
  | Mean of string
  | Time of command
  | Quit

let rec parse_command s =
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
  | [ "mean"; variable ] -> Ok (Mean variable)
  | "mean" :: _ -> Error "Mean requires exactly one argument"
  | "time" :: tail -> (
      match parse_command (String.concat " " tail) with
      | Ok cmd -> Ok (Time cmd)
      | Error e ->
          Error (Printf.sprintf "Failed parsing command after time: %s" e))
  | [ "quit" ] -> Ok Quit
  | "quit" :: _ -> Error "Quit can only stand alone"
  | command :: _ -> Error (Printf.sprintf "%s is not a known command" command)
  | [] -> Error "The command should not be empty"

let program_from_path path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let lexbuf = Lexing.from_channel channel in
      let parse_state = Parser.make_parser lexbuf in
      Parser.parse_program parse_state)

let rec execute state command =
  match command with
  | Observe (variable, value) ->
      let new_state =
        {
          state with
          observation_map = Env.add variable value state.observation_map;
          last_samples = None;
        }
      in
      if Env.mem variable state.observation_map then
        Ok
          ( new_state,
            Printf.sprintf "Replaced observation for %s" variable,
            true )
      else
        Ok (new_state, Printf.sprintf "Added observation for %s" variable, true)
  | Unobserve variable ->
      if Env.mem variable state.observation_map then
        Ok
          ( {
              state with
              observation_map = Env.remove variable state.observation_map;
              last_samples = None;
            },
            Printf.sprintf "Removed observation for %s" variable,
            true )
      else Error (Printf.sprintf "%s had no observation before" variable)
  | Seed s ->
      Ok
        ( { state with rng = Random.State.make [| s |] },
          "Updated random seed",
          true )
  | Sample n -> (
      match state.program with
      | Some program -> (
          match
            Inference.sample state.rng
              { program with observations = Env.bindings state.observation_map }
              n
          with
          | Ok samples ->
              Ok
                ( { state with last_samples = Some samples },
                  "Done sampling",
                  true )
          | Error e -> Error (Eval.string_of_error e))
      | None -> Error "Can not sample without loading a program")
  | Mean variable -> (
      match state.last_samples with
      | Some (traces, weights) ->
          if Env.mem variable (List.hd traces).env then
            let mean =
              List.fold_left2
                (fun acc trace w ->
                  let xn = float_of_value (Env.find variable trace.env) in
                  acc +. (xn *. w))
                0. traces weights
            in
            Ok (state, Printf.sprintf "%s has a mean of %g" variable mean, true)
          else Error (Printf.sprintf "%s is not part of the program" variable)
      | None -> Error "Can only run mean after sampling")
  | Load path -> (
      try
        let program = program_from_path path in
        Ok
          ( {
              state with
              program = Some program;
              last_samples = None;
              observation_map = Env.of_list program.observations;
            },
            "Loaded program",
            true )
      with
      | Sys_error s -> Error s
      | Failure s -> Error s)
  | Time cmd -> (
      let s0 = Sys.time () in
      let res = execute state cmd in
      let s1 = Sys.time () in
      let s_diff = s1 -. s0 in
      match res with
      | Ok (new_state, msg, continue) ->
          Ok
            ( new_state,
              Printf.sprintf "%s\n\tCommand took %g s" msg s_diff,
              continue )
      | Error e -> Error e)
  | Quit -> Ok (state, "", false)
