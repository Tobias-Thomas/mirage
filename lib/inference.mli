val sample :
  Random.State.t ->
  Types.program ->
  int ->
  (Eval.trace list * float list, Eval.error) result
