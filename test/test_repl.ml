open Mirage

let rec show_command (c : Repl.command) =
  match c with
  | Load path -> "Load " ^ path
  | Sample n -> Printf.sprintf "Sample %d" n
  | Seed n -> Printf.sprintf "Seed %d" n
  | Observe (x, v) -> Printf.sprintf "Observe %s %s" x (Types.show_value v)
  | Unobserve x -> "Unobserve " ^ x
  | Mean x -> "Mean " ^ x
  | Time c -> "Time " ^ show_command c
  | Quit -> "Quit"

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
   printing the message or the error, keeping the old state on errors, and
   stopping (like bin/main.ml) when [execute] says not to continue. *)
let run_session lines =
  let rec loop state lines =
    match lines with
    | [] -> ()
    | line :: rest -> (
        Printf.printf "> %s\n" line;
        match Repl.parse_command line with
        | Error msg ->
            print_endline ("error: " ^ msg);
            loop state rest
        | Ok cmd -> (
            match Repl.execute state cmd with
            | Ok (_, _, false) -> print_endline "(session ended)"
            | Ok (state, msg, true) ->
                print_endline msg;
                loop state rest
            | Error msg ->
                print_endline ("error: " ^ msg);
                loop state rest))
  in
  loop (Repl.init ()) lines

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
      "mean p";
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
    Mean p
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
      "mean";
      "mean p c";
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
    error: Mean requires exactly one argument
    error: Mean requires exactly one argument
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

(* mean *)

(* Runs [lines] (which must all succeed), then [mean var], and returns the
   number at the end of the message. *)
let mean_after lines var =
  let state =
    List.fold_left
      (fun state line ->
        match Result.bind (Repl.parse_command line) (Repl.execute state) with
        | Ok (state, _, _) -> state
        | Error msg -> failwith (line ^ ": " ^ msg))
      (Repl.init ()) lines
  in
  match Repl.execute state (Mean var) with
  | Ok (_, msg, _) ->
      float_of_string (List.hd (List.rev (String.split_on_char ' ' msg)))
  | Error msg -> failwith msg

(* With p ~ Uniform(0, 1) and c ~ Bernoulli(p), the posterior of p is
   Beta(2, 1) after observing true (mean 2/3) and Beta(1, 2) after observing
   false (mean 1/3). With 10000 samples the standard error is about 0.003, so
   0.02 leaves plenty of room. *)
let%expect_test "mean matches the analytic posterior" =
  let close expected actual =
    Printf.printf "%b\n" (Float.abs (actual -. expected) < 0.02)
  in
  let load = [ "load " ^ model_file "coin.mrg" coin; "seed 1" ] in
  close (2. /. 3.) (mean_after (load @ [ "sample 10000" ]) "p");
  close (1. /. 3.)
    (mean_after (load @ [ "observe c false"; "sample 10000" ]) "p");
  (* Without the observation the prior mean 1/2 comes back. *)
  close 0.5 (mean_after (load @ [ "unobserve c"; "sample 10000" ]) "p");
  [%expect {|
    true
    true
    true
    |}]

(* Two more conjugate pairs. Beta(2, 2) prior with a Bernoulli observation
   true: posterior Beta(3, 2), mean 3/5. Gamma(2, 1) prior on the rate of an
   Exponential, observing 0.5: posterior Gamma(3, 1.5), mean 2 (posterior sd
   about 1.15, standard error well below 0.05 with 10000 samples). *)
let%expect_test "mean matches conjugate posteriors with gamma and beta priors" =
  let close ~tol expected actual =
    Printf.printf "%b\n" (Float.abs (actual -. expected) < tol)
  in
  let beta_coin = "p ~ Beta(2.0, 2.0);\nc ~ Bernoulli(p);\nobserve c true;\n" in
  let gamma_rate =
    "rate ~ Gamma(2.0, 1.0);\ny ~ Exponential(rate);\nobserve y 0.5;\n"
  in
  close ~tol:0.02 0.6
    (mean_after
       [
         "load " ^ model_file "beta_coin.mrg" beta_coin;
         "seed 1";
         "sample 10000";
       ]
       "p");
  close ~tol:0.1 2.
    (mean_after
       [
         "load " ^ model_file "gamma_rate.mrg" gamma_rate;
         "seed 1";
         "sample 10000";
       ]
       "rate");
  [%expect {|
    true
    true
    |}]

(* A Binary variable counts true as 1, so its mean is the probability of true;
   an observed variable has the same value in every sample. *)
let%expect_test "mean of an observed boolean" =
  Printf.printf "%g\n"
    (mean_after [ "load " ^ model_file "coin.mrg" coin; "sample 100" ] "c");
  [%expect {| 1 |}]

let%expect_test "mean errors" =
  run_session
    [
      "mean p";
      "load " ^ model_file "coin.mrg" coin;
      "mean p";
      "sample 10";
      "mean q";
      "observe c false";
      "mean p";
    ];
  [%expect
    {|
    > mean p
    error: Can only run mean after sampling
    > load coin.mrg
    Loaded program
    > mean p
    error: Can only run mean after sampling
    > sample 10
    Done sampling
    > mean q
    error: q is not part of the program
    > observe c false
    Replaced observation for c
    > mean p
    error: Can only run mean after sampling
    |}]

(* time *)

let%expect_test "parse time" =
  List.iter print_parse
    [ "time sample 10"; "time time sample 10"; "time"; "time smaple 10" ];
  [%expect
    {|
    Time Sample 10
    Time Time Sample 10
    error: Failed parsing command after time: The command should not be empty
    error: Failed parsing command after time: smaple is not a known command
    |}]

(* The duration differs on every run, so timing lines ("Command took <number>
   s") are only checked for their shape; all other lines are printed. *)
let%expect_test "time runs the command and keeps its new state" =
  let state = ref (Repl.init ()) in
  let print_line l =
    if String.starts_with ~prefix:"\tCommand took" l then
      Printf.printf "<timing line ok: %b>\n"
        (Scanf.sscanf_opt l "\tCommand took %f s%!" Fun.id <> None)
    else print_endline l
  in
  let run line =
    match Result.bind (Repl.parse_command line) (Repl.execute !state) with
    | Ok (new_state, msg, _) ->
        state := new_state;
        List.iter print_line (String.split_on_char '\n' msg)
    | Error msg -> print_endline ("error: " ^ msg)
  in
  run ("time load " ^ model_file "coin.mrg" coin);
  run "time seed 1";
  run "time sample 1000";
  (* The samples from the timed command are kept in the state. *)
  run "mean c";
  run "time time mean c";
  run "time unobserve z";
  [%expect
    {|
    Loaded program
    <timing line ok: true>
    Updated random seed
    <timing line ok: true>
    Done sampling
    <timing line ok: true>
    c has a mean of 1
    c has a mean of 1
    <timing line ok: true>
    <timing line ok: true>
    error: z had no observation before
    |}]

(* quit *)

let%expect_test "parse quit" =
  List.iter print_parse [ "quit"; "  quit  "; "quit now"; "time quit" ];
  [%expect
    {|
    Quit
    Quit
    error: Quit can only stand alone
    Time Quit
    |}]

let%expect_test "quit ends the session, also when timed" =
  run_session [ "seed 1"; "quit"; "seed 2" ];
  run_session [ "time quit"; "seed 2" ];
  (* A malformed quit is an ordinary error and the session goes on. *)
  run_session [ "quit now"; "seed 2" ];
  [%expect
    {|
    > seed 1
    Updated random seed
    > quit
    (session ended)
    > time quit
    (session ended)
    > quit now
    error: Quit can only stand alone
    > seed 2
    Updated random seed
    |}]
