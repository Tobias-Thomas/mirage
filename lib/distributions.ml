type distribution =
  | Uniform of float * float (* low, high *)
  | Exponential of float (* lambda *)

let logpdf dist x =
  match dist with
  | Uniform (low, high) ->
      if low < x && x < high then -.log (high -. low) else Float.neg_infinity
  | Exponential lambda ->
      if x < 0. then Float.neg_infinity else log lambda -. (lambda *. x)

let pdf dist x = exp (logpdf dist x)

let cdf dist x =
  match dist with
  | Uniform (low, high) ->
      if x < low then 0.
      else if x > high then 1.
      else (x -. low) /. (high -. low)
  | Exponential lambda -> 1. -. Float.exp (-.lambda *. x)

let quantile dist p =
  match dist with
  | Uniform (low, high) -> low +. (p *. (high -. low))
  | Exponential lambda -> -.log (1. -. p) /. lambda

let sample dist =
  let urand = Random.float 1. in
  quantile dist urand
