open Mirage
open Helpers

(* Deterministic programs *)

let%expect_test "empty program" =
  print_eval "";
  [%expect {| log_weight = 0 |}]

let%expect_test "integer arithmetic" =
  print_eval "a = 1 + 2 * 3; b = 7 / 2; c = -7 / 2; d = 10 - 4 - 3;";
  [%expect
    {|
    a = (Discrete 7)
    b = (Discrete 3)
    c = (Discrete -3)
    d = (Discrete 3)
    log_weight = 0
    |}]

let%expect_test "float arithmetic" =
  print_eval "a = 1.5 * 2.0; b = 1.0 / 4.0; c = -(1.5 + 1.0);";
  [%expect
    {|
    a = (Continuous 3.)
    b = (Continuous 0.25)
    c = (Continuous -2.5)
    log_weight = 0
    |}]

let%expect_test "comparisons" =
  print_eval
    "a = 1 < 2; b = 2.0 > 3.0; c = 1 == 1; d = 0.5 == 0.25; e = true == false;";
  [%expect
    {|
    a = (Binary true)
    b = (Binary false)
    c = (Binary true)
    d = (Binary false)
    e = (Binary false)
    log_weight = 0
    |}]

let%expect_test "variables refer to earlier assignments" =
  print_eval "x = 2; y = x * x; z = y + x;";
  [%expect
    {|
    x = (Discrete 2)
    y = (Discrete 4)
    z = (Discrete 6)
    log_weight = 0
    |}]

let%expect_test "float division by zero" =
  print_eval "a = 1.0 / 0.0; b = 0.0 / 0.0;";
  [%expect
    {|
    a = (Continuous infinity)
    b = (Continuous nan)
    log_weight = 0
    |}]

(* Runtime errors *)

let%expect_test "unbound variable" =
  print_eval "y = x + 1;";
  [%expect
    {| runtime error: Unbound_variable: variable x used before assignment |}]

let%expect_test "mixed int and float" =
  print_eval "x = 1 + 1.0;";
  print_eval "x = 1 == 1.0;";
  [%expect
    {|
    runtime error: Type_error: Both sides of the binary operator must be of the same type
    runtime error: Type_error: Both sides of the binary operator must be of the same type
    |}]

let%expect_test "negating a boolean" =
  print_eval "x = -true;";
  [%expect {| runtime error: Type_error: Binary values can not be negated |}]

(* Unknown names and wrong argument counts are parse errors now (see
   test_parser.ml); only parameter values are checked at runtime. *)
let%expect_test "invalid distribution parameters" =
  print_eval "x ~ Uniform(1.0, 0.0);";
  print_eval "x ~ Exponential(-1.0);";
  print_eval "x ~ Bernoulli(1.5);";
  [%expect
    {|
    runtime error: Invalid_distribution: Uniform(1, 0): expected finite values and low < high
    runtime error: Invalid_distribution: Exponential(-1): expected finite and >0 lambda
    runtime error: Invalid_distribution: Bernoulli(1.5): expected p in [0, 1]
    |}]

(* Parameters can depend on earlier variables, so this can only be caught while
   evaluating, not while parsing. *)
let%expect_test "invalid parameter computed from a variable" =
  print_eval "p = 0.5 + 1.0; c ~ Bernoulli(p);";
  [%expect
    {| runtime error: Invalid_distribution: Bernoulli(1.5): expected p in [0, 1] |}]

let%expect_test "distribution arguments must be floats" =
  print_eval "x ~ Uniform(0, 1);";
  print_eval "x ~ Exponential(1);";
  print_eval "x ~ Bernoulli(true);";
  [%expect
    {|
    runtime error: Type_error: Uniform can only be instantiated with two Floats
    runtime error: Type_error: Exponential can only be instantiated with a Float
    runtime error: Type_error: Bernoulli can only be instantiated with a Float
    |}]

let%expect_test "observing a variable twice" =
  print_eval "x ~ Uniform(0.0, 1.0); observe x 0.5; observe x 0.6;";
  [%expect
    {| runtime error: Redefined_variable: x was observed multiple times |}]

let%expect_test "redefining a variable with =" =
  print_eval "x = 1; x = 2;";
  print_eval "x ~ Uniform(0.0, 1.0); x = 2.0;";
  [%expect
    {|
    runtime error: Redefined_variable: x was defined multiple times
    runtime error: Redefined_variable: x was defined multiple times
    |}]

(* The message is misleading here (see TODO.md); update it with [dune promote]
   once the observed case has its own message. *)
let%expect_test "defining an observed variable with =" =
  print_eval "x = 1.0; observe x 1.0;";
  print_eval "observe x 1.0; x = 1.0;";
  [%expect
    {|
    runtime error: Redefined_variable: x was defined multiple times
    runtime error: Redefined_variable: x was defined multiple times
    |}]

let%expect_test "redefining a variable with ~" =
  print_eval "x = 1; x ~ Uniform(0.0, 1.0);";
  print_eval "x ~ Uniform(0.0, 1.0); x ~ Uniform(5.0, 6.0);";
  [%expect
    {|
    runtime error: Redefined_variable: x was defined multiple times
    runtime error: Redefined_variable: x was defined multiple times
    |}]

(* Known gap (see TODO.md): an observation that no ~ uses is ignored. *)
let%expect_test "observation without a matching ~" =
  print_eval "observe z 1.0;";
  print_eval
    "z = 1.0 + 1.0; x ~ Uniform(0.0, 1.0); observe x 0.5; observe y 1.0;";
  [%expect
    {|
    log_weight = 0
    x = (Continuous 0.5)
    z = (Continuous 2.)
    log_weight = 0
    |}]

(* Known gap (see TODO.md): integer division by zero escapes [eval_program] as
   an OCaml exception instead of returning [Error]. *)
let%expect_test "integer division by zero" =
  print_eval "x = 1 / 0;";
  [%expect {| error: Division_by_zero |}]

(* Sampling *)

let%expect_test "sampling is reproducible with a fixed seed" =
  print_eval "x ~ Uniform(0.0, 1.0); y ~ Exponential(2.0); c ~ Bernoulli(0.5);";
  print_eval "x ~ Uniform(0.0, 1.0); y ~ Exponential(2.0); c ~ Bernoulli(0.5);";
  [%expect
    {|
    c = (Binary true)
    x = (Continuous 0.444764790527)
    y = (Continuous 0.746613822215)
    log_weight = 0
    c = (Binary true)
    x = (Continuous 0.444764790527)
    y = (Continuous 0.746613822215)
    log_weight = 0
    |}]

let%expect_test "distribution arguments can be expressions" =
  print_eval "low = 1.0; x ~ Uniform(low, low * 2.0);";
  [%expect
    {|
    low = (Continuous 1.)
    x = (Continuous 1.44476479053)
    log_weight = 0
    |}]

let seeds = List.init 1000 Fun.id

let%expect_test "uniform samples stay within bounds" =
  sample_var ~seeds "x" "x ~ Uniform(2.0, 3.0);"
  |> List.for_all (function
    | Types.Continuous f -> 2.0 <= f && f <= 3.0
    | _ -> false)
  |> Printf.printf "%b";
  [%expect {| true |}]

let%expect_test "exponential samples are non-negative and finite" =
  sample_var ~seeds "x" "x ~ Exponential(0.5);"
  |> List.for_all (function
    | Types.Continuous f -> f >= 0.0 && Float.is_finite f
    | _ -> false)
  |> Printf.printf "%b";
  [%expect {| true |}]

let%expect_test "bernoulli samples are booleans with roughly the right rate" =
  let samples = sample_var ~seeds "c" "c ~ Bernoulli(0.3);" in
  let trues = List.length (List.filter (( = ) (Types.Binary true)) samples) in
  (* Expected 300; the bounds are about 4 standard deviations wide. *)
  Printf.printf "%b" (240 < trues && trues < 360);
  [%expect {| true |}]

(* Observations *)

let%expect_test "observed variable is bound and scored" =
  print_eval "x ~ Uniform(0.0, 2.0); observe x 0.5;";
  [%expect {|
    x = (Continuous 0.5)
    log_weight = -0.693147
    |}]

let%expect_test "normal observation scored under a sampled mean" =
  print_eval "mu = 1.0; x ~ Normal(mu, 2.0); observe x 3.0;";
  print_eval "x ~ Normal(0.0, 0.0);";
  print_eval "x ~ Normal(0, 1);";
  [%expect
    {|
    mu = (Continuous 1.)
    x = (Continuous 3.)
    log_weight = -2.11209
    runtime error: Invalid_distribution: Normal(0, 0): expected finite values and sigma > 0.
    runtime error: Type_error: Normal can only be instantiated with two Floats
    |}]

let%expect_test "observed boolean" =
  print_eval "c ~ Bernoulli(0.25); observe c true;";
  print_eval "c ~ Bernoulli(0.25); observe c false;";
  [%expect
    {|
    c = (Binary true)
    log_weight = -1.38629
    c = (Binary false)
    log_weight = -0.287682
    |}]

let%expect_test "log weights of several observations add up" =
  print_eval
    "a ~ Bernoulli(0.5); b ~ Bernoulli(0.5); observe a true; observe b false;";
  [%expect
    {|
    a = (Binary true)
    b = (Binary false)
    log_weight = -1.38629
    |}]

let%expect_test "observation outside the support gives weight zero" =
  print_eval "x ~ Uniform(0.0, 1.0); observe x 2.0;";
  [%expect {|
    x = (Continuous 2.)
    log_weight = -inf
    |}]

let%expect_test "observed value is used by later statements" =
  print_eval "x ~ Exponential(1.0); y = x * 2.0; observe x 1.5;";
  [%expect
    {|
    x = (Continuous 1.5)
    y = (Continuous 3.)
    log_weight = -1.5
    |}]

let%expect_test "observe may come before the variable" =
  print_eval "observe x 0.5; x ~ Uniform(0.0, 1.0);";
  [%expect {|
    x = (Continuous 0.5)
    log_weight = 0
    |}]

let%expect_test "observation scored under a sampled parameter" =
  print_eval "p ~ Uniform(0.0, 1.0); c ~ Bernoulli(p); observe c true;";
  [%expect
    {|
    c = (Binary true)
    p = (Continuous 0.444764790527)
    log_weight = -0.81021
    |}]
