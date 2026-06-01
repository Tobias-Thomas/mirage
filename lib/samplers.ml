let sample dist =
  match dist with
  | Distributions.Uniform (low, high) ->
      Types.Continuous (low +. Random.float (high -. low))
  | Distributions.Exponential lambda ->
      let urand = Random.float 1. in
      Types.Continuous (-.log (1. -. urand) /. lambda)
  | Distributions.Bernoulli p ->
      let urand = Random.float 1. in
      Types.Binary (p > urand)
