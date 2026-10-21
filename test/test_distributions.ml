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
