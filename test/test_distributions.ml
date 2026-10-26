open Mirage

(* Reference values computed independently with Python's
   statistics.NormalDist. *)

let normal mu sigma = Result.get_ok (Distributions.normal ~mu ~sigma)
let std = normal 0. 1.
let shifted = normal 1. 2.

let print_quantile d p =
  match Distributions.quantile d p with
  | Some x -> Printf.printf "%.6f\n" x
  | None -> print_endline "None"

let%expect_test "normal: invalid parameters" =
  let show mu sigma =
    match Distributions.normal ~mu ~sigma with
    | Ok _ -> print_endline "Ok"
    | Error msg -> print_endline ("Error: " ^ msg)
  in
  show 0. 0.;
  show 0. (-1.);
  show Float.nan 1.;
  show 0. Float.infinity;
  [%expect
    {|
    Error: Normal(0, 0): expected finite values and sigma > 0.
    Error: Normal(0, -1): expected finite values and sigma > 0.
    Error: Normal(nan, 1): expected finite values and sigma > 0.
    Error: Normal(0, inf): expected finite values and sigma > 0.
    |}]

let%expect_test "normal: logpdf" =
  Printf.printf "%.6f\n" (Distributions.logpdf std 0.);
  Printf.printf "%.6f\n" (Distributions.logpdf std 1.5);
  Printf.printf "%.6f\n" (Distributions.logpdf shifted 3.);
  Printf.printf "%.6f\n" (Distributions.logpdf (normal 0. 0.5) 0.);
  [%expect {|
    -0.918939
    -2.043939
    -2.112086
    -0.225791
    |}]

let%expect_test "normal: cdf" =
  List.iter
    (fun x -> Printf.printf "%.6f\n" (Distributions.cdf std x))
    [ 0.; 1.96; -1.96 ];
  Printf.printf "%.6f\n" (Distributions.cdf shifted 1.);
  Printf.printf "%.6f\n" (Distributions.cdf shifted 3.);
  [%expect
    {|
    0.500000
    0.975002
    0.024998
    0.500000
    0.841345
    |}]

(* Computing 1 + erf(z) loses digits for very negative z (2% error at -8) and
   all of them at -10; the erfc form keeps them. Reference values from
   0.5 * erfc(-z / sqrt 2). *)
let%expect_test "normal: cdf far in the lower tail" =
  Printf.printf "%.3e\n" (Distributions.cdf std (-8.));
  Printf.printf "%.3e\n" (Distributions.cdf std (-10.));
  [%expect {|
    6.221e-16
    7.620e-24
    |}]

let%expect_test "normal: quantile of the standard normal" =
  (* 0.01 and 0.001 lie in Acklam's lower-tail region (p < 0.02425), 0.99 in
     the upper one. *)
  List.iter (print_quantile std) [ 0.5; 0.975; 0.3; 0.01; 0.001; 0.99 ];
  [%expect
    {|
    0.000000
    1.959964
    -0.524401
    -2.326348
    -3.090232
    2.326348
    |}]

let%expect_test "normal: quantile uses mu and sigma" =
  print_quantile shifted 0.5;
  print_quantile shifted 0.975;
  [%expect {|
    1.000000
    4.919928
    |}]

let%expect_test "normal: quantile at and outside the edges" =
  List.iter (print_quantile std) [ 0.; 1.; -0.1; 1.5 ];
  [%expect {|
    -inf
    inf
    None
    None
    |}]

let%expect_test "normal: quantile inverts cdf" =
  let max_error d xs =
    List.fold_left
      (fun acc x ->
        match Distributions.quantile d (Distributions.cdf d x) with
        | Some y -> Float.max acc (Float.abs (y -. x))
        | None -> Float.infinity)
      0. xs
  in
  let xs = [ -3.; -1.; 0.; 0.5; 2.5 ] in
  Printf.printf "%b\n" (max_error std xs < 1e-8);
  Printf.printf "%b\n"
    (max_error shifted (List.map (fun x -> 1. +. (2. *. x)) xs) < 1e-8);
  [%expect {|
    true
    true
    |}]

let%expect_test "normal: samples have the right mean and standard deviation" =
  let rng = Random.State.make [| 42 |] in
  let n = 20_000 in
  let xs = List.init n (fun _ -> Distributions.sample rng shifted) in
  let mean = List.fold_left ( +. ) 0. xs /. float_of_int n in
  let var =
    List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.)) 0. xs
    /. float_of_int (n - 1)
  in
  (* Standard errors are about 0.014 (mean) and 0.010 (std); the bounds are
     several of them wide. *)
  Printf.printf "%b %b %b\n"
    (List.for_all Float.is_finite xs)
    (Float.abs (mean -. 1.) < 0.05)
    (Float.abs (sqrt var -. 2.) < 0.05);
  [%expect {| true true true |}]

(* Gamma (shape alpha, rate beta) and Beta. Reference values come from closed
   forms that hold for particular parameters (computed with Python's math):
   integer shapes give finite sums, shape 1/2 gives erf, Beta(a, 1) = x^a,
   Beta(1, b) = 1 - (1 - x)^b, Beta(1/2, 1/2) = 2/pi asin (sqrt x). *)

let gamma alpha beta = Result.get_ok (Distributions.gamma ~alpha ~beta)
let beta alpha beta = Result.get_ok (Distributions.beta ~alpha ~beta)
let print_floats = List.iter (Printf.printf "%.6f\n")

let%expect_test "gamma and beta: invalid parameters" =
  let show = function
    | Ok _ -> print_endline "Ok"
    | Error msg -> print_endline ("Error: " ^ msg)
  in
  show (Distributions.gamma ~alpha:0. ~beta:1.);
  show (Distributions.gamma ~alpha:1. ~beta:(-1.));
  show (Distributions.beta ~alpha:1. ~beta:0.);
  show (Distributions.beta ~alpha:Float.nan ~beta:1.);
  [%expect
    {|
    Error: Gamma (0, 1): expected positive and finite values
    Error: Gamma (1, -1): expected positive and finite values
    Error: Beta (1, 0): expected positive and finite values
    Error: Beta (nan, 1): expected positive and finite values
    |}]

let%expect_test "gamma: logpdf" =
  print_floats
    [
      Distributions.logpdf (gamma 1. 2.) 0.5;
      Distributions.logpdf (gamma 2. 3.) 1.;
      Distributions.logpdf (gamma 0.5 1.) 0.2;
      Distributions.logpdf (gamma 9. 0.5) 18.;
    ];
  [%expect {|
    -0.306853
    -0.802775
    0.032354
    -2.719953
    |}]

let%expect_test "gamma: logpdf outside the support" =
  print_floats
    [
      Distributions.logpdf (gamma 2. 3.) 0.;
      Distributions.logpdf (gamma 2. 3.) (-1.);
    ];
  [%expect {|
    -inf
    -inf
    |}]

let%expect_test "gamma: cdf" =
  print_floats
    [
      Distributions.cdf (gamma 1. 2.) 0.5;
      Distributions.cdf (gamma 2. 3.) 1.;
      Distributions.cdf (gamma 0.5 1.) 0.2;
      Distributions.cdf (gamma 9. 0.5) 18.;
      Distributions.cdf (gamma 3. 1.) 10.;
      Distributions.cdf (gamma 2. 3.) (-1.);
    ];
  [%expect
    {|
    0.632121
    0.800852
    0.472911
    0.544347
    0.997231
    0.000000
    |}]

let%expect_test "gamma: quantile" =
  (* Gamma(1, 2) is Exponential(2): quantile -log(1 - p) / 2. The median of
     Gamma(1/2, 1) is the x with erf(sqrt x) = 1/2. *)
  List.iter (print_quantile (gamma 1. 2.)) [ 0.1; 0.5; 0.9 ];
  print_quantile (gamma 0.5 1.) 0.5;
  List.iter (print_quantile (gamma 2. 3.)) [ 0.; 1.; 1.5 ];
  [%expect
    {|
    0.052680
    0.346574
    1.151293
    0.227468
    0.000000
    inf
    None
    |}]

let%expect_test "beta: logpdf" =
  print_floats
    [
      Distributions.logpdf (beta 2. 5.) 0.2;
      Distributions.logpdf (beta 0.5 0.5) 0.1;
      Distributions.logpdf (beta 30. 2.) 0.95;
    ];
  [%expect {|
    0.899185
    0.059243
    2.351947
    |}]

let%expect_test "beta: logpdf outside the support" =
  print_floats
    [
      Distributions.logpdf (beta 2. 5.) (-0.1);
      Distributions.logpdf (beta 2. 5.) 1.5;
    ];
  [%expect {|
    -inf
    -inf
    |}]

let%expect_test "beta: cdf" =
  print_floats
    [
      Distributions.cdf (beta 1. 1.) 0.3;
      Distributions.cdf (beta 2.5 1.) 0.3;
      Distributions.cdf (beta 1. 3.5) 0.3;
      Distributions.cdf (beta 2. 5.) 0.2;
      Distributions.cdf (beta 0.5 0.5) 0.1;
      Distributions.cdf (beta 30. 2.) 0.95;
      Distributions.cdf (beta 2. 5.) (-0.5);
      Distributions.cdf (beta 2. 5.) 1.5;
    ];
  [%expect
    {|
    0.300000
    0.049295
    0.713026
    0.344640
    0.204833
    0.536597
    0.000000
    1.000000
    |}]

let%expect_test "beta: quantile" =
  List.iter (print_quantile (beta 2.5 1.)) [ 0.1; 0.5; 0.9 ];
  List.iter (print_quantile (beta 1. 3.5)) [ 0.1; 0.5; 0.9 ];
  List.iter (print_quantile (beta 0.5 0.5)) [ 0.1; 0.5; 0.9 ];
  List.iter (print_quantile (beta 2. 5.)) [ 0.; 1.; -0.1 ];
  [%expect
    {|
    0.398107
    0.757858
    0.958732
    0.029654
    0.179665
    0.482053
    0.024472
    0.500000
    0.975528
    0.000000
    1.000000
    None
    |}]

(* For x far in the upper tail the cdf rounds to exactly 1 and the quantile is
   inf, so the x values are chosen where the cdf is still below 1. *)
let%expect_test "gamma and beta: quantile inverts cdf" =
  let max_rel_error d xs =
    List.fold_left
      (fun acc x ->
        match Distributions.quantile d (Distributions.cdf d x) with
        | Some y -> Float.max acc (Float.abs (y -. x) /. x)
        | None -> Float.infinity)
      0. xs
  in
  List.iter
    (fun (d, xs) -> Printf.printf "%b\n" (max_rel_error d xs < 1e-6))
    [
      (gamma 0.5 1., [ 0.01; 0.3; 1.; 4. ]);
      (gamma 2. 3., [ 0.05; 0.5; 1.; 2. ]);
      (gamma 9. 0.5, [ 5.; 18.; 30. ]);
      (beta 0.5 0.5, [ 0.01; 0.2; 0.5; 0.8; 0.99 ]);
      (beta 2. 5., [ 0.05; 0.2; 0.5; 0.8 ]);
      (beta 30. 2., [ 0.8; 0.9; 0.95; 0.99 ]);
    ];
  [%expect {|
    true
    true
    true
    true
    true
    true
    |}]

let mean_and_variance xs =
  let n = float_of_int (List.length xs) in
  let mean = List.fold_left ( +. ) 0. xs /. n in
  let var =
    List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.)) 0. xs /. (n -. 1.)
  in
  (mean, var)

(* With 50000 samples the sample mean is within about 1% and the sample
   variance within about 2% of the exact values (one standard error for the
   worst case below); the bounds of 3% and 6% are several standard errors. *)
let%expect_test "gamma and beta: samples have the right mean and variance" =
  let rng = Random.State.make [| 42 |] in
  let check name d ~mean ~var =
    let xs = List.init 50_000 (fun _ -> Distributions.sample rng d) in
    let m, v = mean_and_variance xs in
    Printf.printf "%s: finite %b, mean %b, variance %b\n" name
      (List.for_all Float.is_finite xs)
      (Float.abs (m -. mean) < 0.03 *. mean)
      (Float.abs (v -. var) < 0.06 *. var)
  in
  check "Gamma(0.5, 1)" (gamma 0.5 1.) ~mean:0.5 ~var:0.5;
  check "Gamma(2, 3)" (gamma 2. 3.) ~mean:(2. /. 3.) ~var:(2. /. 9.);
  check "Gamma(9, 0.5)" (gamma 9. 0.5) ~mean:18. ~var:36.;
  check "Beta(0.5, 0.5)" (beta 0.5 0.5) ~mean:0.5 ~var:0.125;
  check "Beta(2, 5)" (beta 2. 5.) ~mean:(2. /. 7.) ~var:(10. /. 392.);
  check "Beta(30, 2)" (beta 30. 2.) ~mean:(30. /. 32.)
    ~var:(60. /. (1024. *. 33.));
  [%expect
    {|
    Gamma(0.5, 1): finite true, mean true, variance true
    Gamma(2, 3): finite true, mean true, variance true
    Gamma(9, 0.5): finite true, mean true, variance true
    Beta(0.5, 0.5): finite true, mean true, variance true
    Beta(2, 5): finite true, mean true, variance true
    Beta(30, 2): finite true, mean true, variance true
    |}]
