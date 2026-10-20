open Mirage

let rec repl state =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | line -> (
      match Repl.parse_command line with
      | Error msg ->
          prerr_endline msg;
          repl state
      | Ok cmd -> (
          match Repl.execute state cmd with
          | Error msg ->
              prerr_endline msg;
              repl state
          | Ok (new_state, message) ->
              print_string "\t";
              print_string message;
              print_string "\n";
              flush stdout;
              repl new_state))

let () =
  let state = Repl.init () in
  repl state
