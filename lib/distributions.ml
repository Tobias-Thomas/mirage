type distribution =
  | Uniform of float * float (* low, high *)
  | Exponential of float (* lambda *)
  | Bernoulli of float (* p *)

let logpdf dist x =
  match dist with
  | Uniform (low, high) ->
      if low < x && x < high then -.log (high -. low) else Float.neg_infinity
  | Exponential lambda ->
      if x < 0. then Float.neg_infinity else log lambda -. (lambda *. x)
  | Bernoulli p ->
      if x == 0. then 1. -. p else if x == 1. then p else Float.neg_infinity

let pdf dist x = exp (logpdf dist x)

let cdf dist x =
  match dist with
  | Uniform (low, high) ->
      if x < low then 0.
      else if x > high then 1.
      else (x -. low) /. (high -. low)
  | Exponential lambda -> 1. -. Float.exp (-.lambda *. x)
  | Bernoulli p -> if x < 0. then 0. else if x < 1. then 1. -. p else 1.

let quantile dist p =
  match dist with
  | Uniform (low, high) -> Some (low +. (p *. (high -. low)))
  | Exponential lambda -> Some (-.log (1. -. p) /. lambda)
  | Bernoulli p -> None
