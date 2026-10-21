let a =
  [|
    -3.969683028665376e+01;
    2.209460984245205e+02;
    -2.759285104469687e+02;
    1.383577518672690e+02;
    -3.066479806614716e+01;
    2.506628277459239e+00;
  |]

let b =
  [|
    -5.447609879822406e+01;
    1.615858368580409e+02;
    -1.556989798598866e+02;
    6.680131188771972e+01;
    -1.328068155288572e+01;
  |]

let c =
  [|
    -7.784894002430293e-03;
    -3.223964580411365e-01;
    -2.400758277161838e+00;
    -2.549732539343734e+00;
    4.374664141464968e+00;
    2.938163982698783e+00;
  |]

let d =
  [|
    7.784695709041462e-03;
    3.224671290700398e-01;
    2.445134137142996e+00;
    3.754408661907416e+00;
  |]

let acklam_approximation p =
  let horners_method coefficients =
   fun x -> Array.fold_left (fun acc c -> (x *. acc) +. c) 0. coefficients
  in
  let plow = 0.02425 in
  let phigh = 1. -. plow in
  if p < plow then
    let q = sqrt (-2. *. log p) in
    horners_method c q /. ((horners_method d q *. q) +. 1.)
  else if phigh < p then
    let q = sqrt (-2. *. log (1. -. p)) in
    -.horners_method c q /. ((horners_method d q *. q) +. 1.)
  else
    let q = p -. 0.5 in
    let r = q *. q in
    horners_method a r *. q /. ((horners_method b r *. r) +. 1.)
