val invert_cdf :
  ?tol:float ->
  ?max_iter:int ->
  (float -> 'a) ->
  'a ->
  low:float ->
  high:float ->
  float

val probit : float -> float
val loggamma : float -> float
val regularized_gamma_p : float -> float -> float
val regularized_beta : float -> float -> float -> float
