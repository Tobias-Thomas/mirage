let sample rng dist =
  match dist with
  | Distributions.Uniform (low, high) ->
      Types.Continuous (low +. Random.State.float rng (high -. low))
  | Distributions.Exponential lambda ->
      let urand = Random.State.float rng 1. in
      Types.Continuous (-.log (1. -. urand) /. lambda)
  | Distributions.Bernoulli p ->
      let urand = Random.State.float rng 1. in
      Types.Binary (p > urand)
