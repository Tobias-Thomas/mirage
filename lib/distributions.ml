open Special_functions

type _ distribution =
  | Uniform : float * float -> float distribution
  | Exponential : float -> float distribution
  | Bernoulli : float -> bool distribution
  | Normal : float * float -> float distribution
  | Gamma : float * float -> float distribution
  | Beta : float * float -> float distribution

type any_distribution = Any : 'a distribution -> any_distribution
type _ kind = Float : float kind | Bool : bool kind

let kind : type a. a distribution -> a kind = function
  | Uniform _ -> Float
  | Exponential _ -> Float
  | Bernoulli _ -> Bool
  | Normal _ -> Float
  | Gamma _ -> Float
  | Beta _ -> Float

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

let normal ~mu ~sigma =
  if Float.is_finite mu && Float.is_finite sigma && sigma > 0. then
    Ok (Normal (mu, sigma))
  else
    Error
      (Printf.sprintf "Normal(%g, %g): expected finite values and sigma > 0." mu
         sigma)

let gamma ~alpha ~beta =
  if Float.is_finite alpha && Float.is_finite beta && alpha > 0. && beta > 0.
  then Ok (Gamma (alpha, beta))
  else
    Error
      (Printf.sprintf "Gamma (%g, %g): expected positive and finite values"
         alpha beta)

let beta ~alpha ~beta =
  if Float.is_finite alpha && Float.is_finite beta && alpha > 0. && beta > 0.
  then Ok (Beta (alpha, beta))
  else
    Error
      (Printf.sprintf "Beta (%g, %g): expected positive and finite values" alpha
         beta)

let logpdf : type a. a distribution -> a -> float =
 fun dist x ->
  match dist with
  | Uniform (low, high) ->
      if low <= x && x <= high then -.log (high -. low) else Float.neg_infinity
  | Exponential lambda ->
      if x < 0. then Float.neg_infinity else log lambda -. (lambda *. x)
  | Bernoulli p -> if x then log p else log (1. -. p)
  | Normal (mu, sigma) ->
      -.(log (2. *. Float.pi) /. 2.)
      -. log sigma
      -. (((x -. mu) ** 2.) /. (2. *. (sigma ** 2.)))
  | Gamma (alpha, beta) ->
      if x <= 0. then Float.neg_infinity
      else
        (alpha *. log beta)
        -. loggamma alpha
        +. ((alpha -. 1.) *. log x)
        -. (beta *. x)
  | Beta (alpha, beta) ->
      if x <= 0. || x >= 1. then Float.neg_infinity
      else
        ((alpha -. 1.) *. log x)
        +. ((beta -. 1.) *. log (1. -. x))
        -. (loggamma alpha +. loggamma beta -. loggamma (alpha +. beta))

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
  | Normal (mu, sigma) -> 0.5 *. Float.erfc (-.(x -. mu) /. (sigma *. sqrt 2.))
  | Gamma (alpha, beta) -> regularized_gamma_p alpha (beta *. x)
  | Beta (alpha, beta) -> regularized_beta x alpha beta

let quantile : type a. a distribution -> float -> a option =
 fun dist p ->
  if p >= 0. && p <= 1. then
    match dist with
    | Uniform (low, high) -> Some (low +. (p *. (high -. low)))
    | Exponential lambda -> Some (-.log (1. -. p) /. lambda)
    | Bernoulli q -> Some (p > 1. -. q)
    | Normal (mu, sigma) ->
        if p = 0. then Some Float.neg_infinity
        else if p = 1. then Some Float.infinity
        else Some (mu +. (probit p *. sigma))
    | Gamma (alpha, beta) ->
        if p = 0. then Some 0.
        else if p = 1. then Some Float.infinity
        else
          let cdf x = regularized_gamma_p alpha (beta *. x) in
          let rec upper h = if cdf h < p then upper (2. *. h) else h in
          Some (invert_cdf cdf p ~low:0. ~high:(upper (alpha /. beta)))
    | Beta (alpha, beta) ->
        if p = 0. then Some 0.
        else if p = 1. then Some 1.
        else
          Some
            (invert_cdf
               (fun x -> regularized_beta x alpha beta)
               p ~low:0. ~high:1.)
  else None

let rec uniform_open rng =
  let u = Random.State.float rng 1. in
  if u > 0. && u < 1. then u else uniform_open rng

let standard_normal rng =
  let u1 = uniform_open rng in
  let u2 = uniform_open rng in
  sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2)

let rec standard_gamma rng alpha =
  if alpha < 1. then
    standard_gamma rng (alpha +. 1.) *. (uniform_open rng ** (1. /. alpha))
  else
    let d = alpha -. (1. /. 3.) in
    let c = 1. /. sqrt (9. *. d) in
    let rec attempt () =
      let x = standard_normal rng in
      let v = 1. +. (c *. x) in
      if v <= 0. then attempt ()
      else
        let v = v *. v *. v in
        let u = uniform_open rng in
        if log u < (0.5 *. x *. x) +. d -. (d *. v) +. (d *. log v) then d *. v
        else attempt ()
    in
    attempt ()

let sample : type a. Random.State.t -> a distribution -> a =
 fun rng dist ->
  match dist with
  | Uniform (low, high) -> low +. (uniform_open rng *. (high -. low))
  | Exponential lambda ->
      let urand = uniform_open rng in
      -.log (1. -. urand) /. lambda
  | Bernoulli p ->
      let urand = uniform_open rng in
      p > urand
  | Normal (mu, sigma) ->
      let z = standard_normal rng in
      mu +. (z *. sigma)
  | Gamma (alpha, beta) -> standard_gamma rng alpha /. beta
  | Beta (alpha, beta) ->
      let x = standard_gamma rng alpha in
      let y = standard_gamma rng beta in
      x /. (x +. y)
