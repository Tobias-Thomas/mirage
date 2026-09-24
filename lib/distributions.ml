type distribution =
  | Uniform of float * float (* low, high *)
  | Exponential of float (* lambda *)
  | Bernoulli of float (* p *)

let uniform ~low ~high =
  if Float.is_finite low && Float.is_finite high && low < high then
    Ok (Uniform (low, high))
  else
    Error
      (Printf.sprintf "Uniform(%g, %g): expected finite values and low < high"
         low high)

let exponential ~lambda =
  if Float.is_finite lambda && lambda > 0. then Ok (Exponential lambda)
  else
    Error
      (Printf.sprintf "Exponential(%g): expected finite and >0 lambda" lambda)

let bernoulli ~p =
  if p >= 0. && p <= 1. then Ok (Bernoulli p)
  else Error (Printf.sprintf "Bernoulli(%g): expected p in [0, 1]" p)

let logpdf dist x =
  match dist with
  | Uniform (low, high) ->
      if low < x && x < high then -.log (high -. low) else Float.neg_infinity
  | Exponential lambda ->
      if x < 0. then Float.neg_infinity else log lambda -. (lambda *. x)
  | Bernoulli p ->
      if x = 0. then log (1. -. p)
      else if x = 1. then log p
      else Float.neg_infinity

let pdf dist x = exp (logpdf dist x)

let cdf dist x =
  match dist with
  | Uniform (low, high) ->
      if x < low then 0.
      else if x > high then 1.
      else (x -. low) /. (high -. low)
  | Exponential lambda -> if x < 0. then 0. else 1. -. Float.exp (-.lambda *. x)
  | Bernoulli p -> if x < 0. then 0. else if x < 1. then 1. -. p else 1.

let quantile dist p =
  match dist with
  | Uniform (low, high) ->
      if p >= low && p <= high then Some (low +. (p *. (high -. low))) else None
  | Exponential lambda -> Some (-.log (1. -. p) /. lambda)
  | Bernoulli _ -> None
