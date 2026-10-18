type _ distribution =
  | Uniform : float * float -> float distribution
  | Exponential : float -> float distribution
  | Bernoulli : float -> bool distribution

type any_distribution = Any : 'a distribution -> any_distribution
type _ kind = Float : float kind | Bool : bool kind

let kind : type a. a distribution -> a kind = function
  | Uniform _ -> Float
  | Exponential _ -> Float
  | Bernoulli _ -> Bool

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

let logpdf : type a. a distribution -> a -> float =
 fun dist x ->
  match dist with
  | Uniform (low, high) ->
      if low <= x && x <= high then -.log (high -. low) else Float.neg_infinity
  | Exponential lambda ->
      if x < 0. then Float.neg_infinity else log lambda -. (lambda *. x)
  | Bernoulli p -> if x then log p else log (1. -. p)

let pdf dist x = exp (logpdf dist x)

let cdf : type a. a distribution -> a -> float =
 fun dist x ->
  match dist with
  | Uniform (low, high) ->
      if x < low then 0.
      else if x > high then 1.
      else (x -. low) /. (high -. low)
  | Exponential lambda -> if x < 0. then 0. else 1. -. Float.exp (-.lambda *. x)
  | Bernoulli p -> if x then 1. else 1. -. p

let quantile : type a. a distribution -> float -> a option =
 fun dist p ->
  if p >= 0. && p <= 1. then
    match dist with
    | Uniform (low, high) -> Some (low +. (p *. (high -. low)))
    | Exponential lambda -> Some (-.log (1. -. p) /. lambda)
    | Bernoulli q -> Some (p > 1. -. q)
  else None

let sample : type a. Random.State.t -> a distribution -> a =
 fun rng dist ->
  match dist with
  | Uniform (low, high) -> low +. Random.State.float rng (high -. low)
  | Exponential lambda ->
      let urand = Random.State.float rng 1. in
      -.log (1. -. urand) /. lambda
  | Bernoulli p ->
      let urand = Random.State.float rng 1. in
      p > urand
