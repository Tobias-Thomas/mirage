let invert_cdf ?(tol = 1e-12) ?(max_iter = 200) cdf p ~low ~high =
  let rec bisect low high i =
    let mid = 0.5 *. (low +. high) in
    if high -. low <= tol *. Float.max 1. mid || i >= max_iter then mid
    else if cdf mid < p then bisect mid high (i + 1)
    else bisect low mid (i + 1)
  in
  bisect low high 0

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

let probit p =
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

let g = 7.

let p =
  [|
    0.99999999999980993;
    676.5203681218851;
    -1259.1392167224028;
    771.32342877765313;
    -176.61502916214059;
    12.507343278686905;
    -0.13857109526572012;
    9.9843695780195716e-6;
    1.5056327351493116e-7;
  |]

let loggamma_lanczos z =
  let rec a s i =
    if i >= Array.length p then s
    else a (s +. (p.(i) /. (z +. float_of_int i -. 1.))) (i + 1)
  in
  (log (2. *. Float.pi) /. 2.)
  +. ((z -. 0.5) *. log (z +. g -. 0.5))
  -. (z +. g -. 0.5)
  +. log (a p.(0) 1)

let rec loggamma z =
  if z < 0.5 then
    log Float.pi -. log (Float.abs (sin (Float.pi *. z))) -. loggamma (1. -. z)
  else loggamma_lanczos z

let tiny = 1e-300

let cont_fraction ?(eps = 1e-15) ?(max_iter = 1000) ~b0 coefficients =
  let floor v = if Float.abs v < tiny then tiny else v in
  let rec step i c d h =
    let a_i, b_i = coefficients i in
    let d = 1. /. floor (b_i +. (a_i *. d)) in
    let c = floor (b_i +. (a_i /. c)) in
    let delta = c *. d in
    let h = h *. delta in
    if Float.abs (delta -. 1.) < eps || i >= max_iter then h
    else step (i + 1) c d h
  in
  let h0 = floor b0 in
  step 1 h0 0. h0

let regularized_gamma_p a x =
  if x <= 0. then 0.
  else if x = Float.infinity then 1.
  else
    let eps = 1e-15 in
    let rec p sum term n =
      if abs_float term <= eps *. abs_float sum || n > 1000. then sum
      else
        let new_term = term *. x /. (a +. n) in
        p (sum +. new_term) new_term (n +. 1.)
    in
    let pre = exp (-.x +. (a *. log x) -. loggamma a) in
    if x < a +. 1. then pre *. p (1. /. a) (1. /. a) 1.
    else
      1.
      -. pre
         /. cont_fraction
              ~b0:(x +. 1. -. a)
              (fun i ->
                let i = float_of_int i in
                (-.i *. (i -. a), x +. 1. -. a +. (2. *. i)))

let regularized_beta x a b =
  if x <= 0. then 0.
  else if x >= 1. then 1.
  else
    let front =
      exp
        (loggamma (a +. b)
        -. loggamma a -. loggamma b
        +. (a *. log x)
        +. (b *. log (1. -. x)))
    in
    let continued_fraction a b x =
      let coefficient i =
        let m = float_of_int (i / 2) in
        let d =
          if i mod 2 = 0 then
            m *. (b -. m) *. x /. ((a +. (2. *. m) -. 1.) *. (a +. (2. *. m)))
          else
            -.(a +. m)
            *. (a +. b +. m)
            *. x
            /. ((a +. (2. *. m)) *. (a +. (2. *. m) +. 1.))
        in
        (d, 1.)
      in
      1. /. cont_fraction ~b0:1. coefficient
    in
    if x < (a +. 1.) /. (a +. b +. 2.) then
      front *. continued_fraction a b x /. a
    else 1. -. (front *. continued_fraction b a (1. -. x) /. b)
