type value = Continuous of float | Discrete of int | Binary of bool

let sample dist =
  match dist with
  | Distributions.Uniform (low, high) ->
      Continuous (low +. Random.float (high -. low))
  | Distributions.Exponential lambda ->
      let urand = Random.float 1. in
      Continuous (-.log (1. -. urand) /. lambda)
  | Distributions.Bernoulli p ->
      let urand = Random.float 1. in
      Binary (p > urand)
