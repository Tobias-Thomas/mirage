type distribution = private
  | Uniform of float * float (* low, high *)
  | Exponential of float (* lambda *)
  | Bernoulli of float (* p *)

val uniform: low:float -> high:float -> (distribution, string) result
val exponential: lambda:float -> (distribution, string) result
val bernoulli: p:float -> (distribution, string) result

val logpdf: distribution -> float -> float
val pdf: distribution -> float -> float
val cdf: distribution ->  float -> float
val quantile: distribution -> float -> float option
