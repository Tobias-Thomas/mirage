open Mirage

let show_command (c : Repl.command) =
  match c with
  | Load path -> "Load " ^ path
  | Sample n -> Printf.sprintf "Sample %d" n
  | Seed n -> Printf.sprintf "Seed %d" n
  | Observe (x, v) -> Printf.sprintf "Observe %s %s" x (Types.show_value v)
  | Unobserve x -> "Unobserve " ^ x

let print_parse s =
  match Repl.parse_command s with
  | Ok c -> print_endline (show_command c)
  | Error msg -> print_endline ("error: " ^ msg)

(* Writes [contents] to [name] in the test's working directory (a dune
   sandbox) and returns the name. Relative names keep the output stable. *)
let model_file name contents =
  Out_channel.with_open_text name (fun oc -> output_string oc contents);
  name

(* Runs each line through [parse_command] and [execute] like the REPL does,
   printing the message or the error, and keeping the old state on errors. *)
let run_session lines =
  ignore
    (List.fold_left
       (fun state line ->
         Printf.printf "> %s\n" line;
         match Repl.parse_command line with
         | Error msg ->
             print_endline ("error: " ^ msg);
             state
         | Ok cmd -> (
             match Repl.execute state cmd with
             | Ok (state, msg) ->
                 print_endline msg;
                 state
             | Error msg ->
                 print_endline ("error: " ^ msg);
                 state))
       (Repl.init ()) lines)

let coin = "p ~ Uniform(0.0, 1.0);\nc ~ Bernoulli(p);\nobserve c true;\n"

(* parse_command *)

let%expect_test "parse valid commands" =
  List.iter print_parse
    [
      "load model.mrg";
      "sample 100";
      "seed 42";
      "seed -1";
      "observe x 2.5";
      "observe x -2.5";
      "observe n 3";
      "observe c true";
      "unobserve x";
    ];
  [%expect
    {|
    Load model.mrg
    Sample 100
    Seed 42
    Seed -1
    Observe x (Continuous 2.5)
    Observe x (Continuous -2.5)
    Observe n (Discrete 3)
    Observe c (Binary true)
    Unobserve x
    |}]

let%expect_test "extra spaces are ignored" =
  print_parse "  sample    10  ";
  [%expect {| Sample 10 |}]

let%expect_test "parse invalid commands" =
  List.iter print_parse
    [
      "";
      "   ";
      "smaple 10";
      "Sample 10";
      "load";
      "load a b";
      "sample";
      "sample ten";
      "sample 0";
      "sample -5";
      "sample 1 2";
      "seed 1.5";
      "observe x";
      "observe x y";
      "unobserve";
    ];
  [%expect
    {|
    error: The command should not be empty
    error: The command should not be empty
    error: smaple is not a known command
    error: Sample is not a known command
    error: Load requires exactly one argument
    error: Load requires exactly one argument
    error: Sample requires exactly one positive integer argument
    error: Sample requires exactly one positive integer argument
    error: Sample requires exactly one positive integer argument
    error: Sample requires exactly one positive integer argument
    error: Sample requires exactly one positive integer argument
    error: Seed requires exactly one integer argument
    error: Observe requires exactly two arguments
    error: y must parse as a concrete value
    error: Unobserve requires exactly one argument
    |}]

(* Known gap (see TODO.md): the rest of the value string is not checked, and
   parser failures escape as exceptions instead of returning [Error]. *)
let%expect_test "observe with a malformed value" =
  print_parse "observe x 1.5abc";
  (match Repl.parse_command "observe x )" with
  | _ -> print_endline "no exception"
  | exception e -> print_endline ("exception: " ^ Printexc.to_string e));
  [%expect
    {|
    Observe x (Continuous 1.5)
    exception: Failure("wrong token")
    |}]

(* execute *)

let%expect_test "commands that need a program" =
  run_session [ "sample 10" ];
  [%expect
    {|
    > sample 10
    error: Can not sample without loading a program
    |}]

let%expect_test "load and sample" =
  run_session [ "load " ^ model_file "coin.mrg" coin; "seed 1"; "sample 100" ];
  [%expect
    {|
    > load coin.mrg
    Loaded program
    > seed 1
    Updated random seed
    > sample 100
    Done sampling
    |}]

let%expect_test "load errors keep the session going" =
  run_session
    [
      "load /does/not/exist.mrg";
      "load " ^ model_file "syntax_error.mrg" "p ~ Uniform(0.0 1.0);";
      "sample 10";
      "load " ^ model_file "coin.mrg" coin;
      "sample 10";
    ];
  [%expect
    {|
    > load /does/not/exist.mrg
    error: /does/not/exist.mrg: No such file or directory
    > load syntax_error.mrg
    error: wrong token
    > sample 10
    error: Can not sample without loading a program
    > load coin.mrg
    Loaded program
    > sample 10
    Done sampling
    |}]

let%expect_test "observations from the file can be replaced and removed" =
  run_session
    [
      "load " ^ model_file "coin.mrg" coin;
      "observe c false";
      "unobserve c";
      "unobserve c";
      "sample 10";
    ];
  [%expect
    {|
    > load coin.mrg
    Loaded program
    > observe c false
    Replaced observation for c
    > unobserve c
    Removed observation for c
    > unobserve c
    error: c had no observation before
    > sample 10
    Done sampling
    |}]

let%expect_test "observations can be added before loading" =
  run_session [ "observe x 1.0"; "observe x 2.0"; "unobserve x" ];
  [%expect
    {|
    > observe x 1.0
    Added observation for x
    > observe x 2.0
    Replaced observation for x
    > unobserve x
    Removed observation for x
    |}]

let%expect_test "loading replaces the previous observations" =
  run_session
    [ "observe z 1.0"; "load " ^ model_file "coin.mrg" coin; "unobserve z" ];
  [%expect
    {|
    > observe z 1.0
    Added observation for z
    > load coin.mrg
    Loaded program
    > unobserve z
    error: z had no observation before
    |}]

let%expect_test "evaluation errors are reported by sample" =
  run_session
    [
      "load " ^ model_file "coin.mrg" coin;
      "observe c 1";
      "sample 10";
      "load " ^ model_file "bad_param.mrg" "p = 2.0; c ~ Bernoulli(p);";
      "sample 10";
    ];
  [%expect
    {|
    > load coin.mrg
    Loaded program
    > observe c 1
    Replaced observation for c
    > sample 10
    error: Type Error: Conversion failed
    > load bad_param.mrg
    Loaded program
    > sample 10
    error: Invalid Distribution: Bernoulli(2): expected p in [0, 1]
    |}]
