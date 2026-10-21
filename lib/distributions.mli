type _ distribution = private
  | Uniform : float * float -> float distribution
  | Exponential : float -> float distribution
  | Bernoulli : float -> bool distribution
  | Normal : float * float -> float distribution

type any_distribution = Any : 'a distribution -> any_distribution
type _ kind = Float : float kind | Bool : bool kind

val kind : 'a distribution -> 'a kind
val uniform : low:float -> high:float -> (float distribution, string) result
val exponential : lambda:float -> (float distribution, string) result
val bernoulli : p:float -> (bool distribution, string) result
val normal : mu:float -> sigma:float -> (float distribution, string) result
val logpdf : 'a distribution -> 'a -> float
val pdf : 'a distribution -> 'a -> float
val cdf : 'a distribution -> 'a -> float
val quantile : 'a distribution -> float -> 'a option
val sample : Random.State.t -> 'a distribution -> 'a
