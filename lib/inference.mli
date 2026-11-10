val likelihood_weighting :
  Random.State.t ->
  Types.program ->
  int ->
  (Eval.trace list * float list, Eval.error) result

val metropolis_hastings :
  Random.State.t ->
  Types.program ->
  int ->
  (Eval.trace list * float list, Eval.error) result
