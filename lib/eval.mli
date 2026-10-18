type error =
  | Unbound_variable of string
  | Redefined_variable of string
  | Type_error of string
  | Invalid_distribution of string

exception Runtime_error of error

module Env : Map.S with type key = string

type trace = { env : Types.value Env.t; log_weight : float }

val eval_program : Random.State.t -> Types.program -> (trace, error) result
