let sample : type a. Random.State.t -> a Distributions.distribution -> a =
 fun rng dist ->
  match dist with
  | Distributions.Uniform (low, high) ->
      low +. Random.State.float rng (high -. low)
  | Distributions.Exponential lambda ->
      let urand = Random.State.float rng 1. in
      -.log (1. -. urand) /. lambda
  | Distributions.Bernoulli p ->
      let urand = Random.State.float rng 1. in
      p > urand
