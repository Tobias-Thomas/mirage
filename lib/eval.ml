open Types
open Distributions

type error =
  | Unbound_variable of string
  | Redefined_variable of string
  | Type_error of string
  | Invalid_distribution of string

let string_of_error e =
  match e with
  | Unbound_variable e -> Printf.sprintf "Unbound Variable: %s" e
  | Redefined_variable e -> Printf.sprintf "Redefined Variable: %s" e
  | Type_error e -> Printf.sprintf "Type Error: %s" e
  | Invalid_distribution e -> Printf.sprintf "Invalid Distribution: %s" e

exception Runtime_error of error

module Env = Map.Make (String)

type trace = { env : value Env.t; log_weight : float }

let to_value : type a. a kind -> a -> value =
 fun kind v -> match kind with Float -> Continuous v | Bool -> Binary v

let of_value : type a. Types.value -> a kind -> a =
 fun v kind ->
  match (kind, v) with
  | Float, Continuous f -> f
  | Bool, Binary b -> b
  | _ -> raise (Runtime_error (Type_error "Conversion failed"))

let rec eval_expr env expr =
  match expr with
  | Literal v -> v
  | Var v -> (
      match Env.find_opt v env with
      | Some res -> res
      | None ->
          raise
            (Runtime_error
               (Unbound_variable
                  (Printf.sprintf "variable %s used before assignment" v))))
  | UnOp (op, e) -> (
      let v = eval_expr env e in
      match (op, v) with
      | Neg, Continuous f -> Continuous (-.f)
      | Neg, Discrete i -> Discrete (-i)
      | Neg, Binary _ ->
          raise (Runtime_error (Type_error "Binary values can not be negated")))
  | BinOp (op, lhs, rhs) -> (
      let lhv = eval_expr env lhs in
      let rhv = eval_expr env rhs in
      match (op, lhv, rhv) with
      | Add, Continuous l, Continuous r -> Continuous (l +. r)
      | Add, Discrete l, Discrete r -> Discrete (l + r)
      | Sub, Continuous l, Continuous r -> Continuous (l -. r)
      | Sub, Discrete l, Discrete r -> Discrete (l - r)
      | Mul, Continuous l, Continuous r -> Continuous (l *. r)
      | Mul, Discrete l, Discrete r -> Discrete (l * r)
      | Div, Continuous l, Continuous r -> Continuous (l /. r)
      (* Note that this is integer division *)
      | Div, Discrete l, Discrete r -> Discrete (l / r)
      | Gt, Continuous l, Continuous r -> Binary (l > r)
      | Gt, Discrete l, Discrete r -> Binary (l > r)
      | Lt, Continuous l, Continuous r -> Binary (l < r)
      | Lt, Discrete l, Discrete r -> Binary (l < r)
      | Eq, Discrete l, Discrete r -> Binary (l = r)
      | Eq, Continuous l, Continuous r -> Binary (l = r)
      | Eq, Binary l, Binary r -> Binary (l = r)
      | _ ->
          raise
            (Runtime_error
               (Type_error
                  "Both sides of the binary operator must be of the same type"))
      )

let of_result_dist : type a. (a distribution, string) result -> any_distribution
    =
 fun dist_res ->
  match dist_res with
  | Ok d -> Any d
  | Error e -> raise (Runtime_error (Invalid_distribution e))

let eval_dist env dist =
  match dist with
  | PBernoulli p -> (
      match eval_expr env p with
      | Continuous f -> of_result_dist (Distributions.bernoulli ~p:f)
      | _ ->
          raise
            (Runtime_error
               (Type_error "Bernoulli can only be instantiated with a Float")))
  | PExponential lambda -> (
      match eval_expr env lambda with
      | Continuous f -> of_result_dist (Distributions.exponential ~lambda:f)
      | _ ->
          raise
            (Runtime_error
               (Type_error "Exponential can only be instantiated with a Float"))
      )
  | PUniform (low, high) -> (
      match (eval_expr env low, eval_expr env high) with
      | Continuous f1, Continuous f2 ->
          of_result_dist (Distributions.uniform ~low:f1 ~high:f2)
      | _ ->
          raise
            (Runtime_error
               (Type_error "Uniform can only be instantiated with two Floats")))
  | PNormal (mu, sigma) -> (
      match (eval_expr env mu, eval_expr env sigma) with
      | Continuous f1, Continuous f2 ->
          of_result_dist (Distributions.normal ~mu:f1 ~sigma:f2)
      | _ ->
          raise
            (Runtime_error
               (Type_error "Normal can only be instantiated with two Floats")))

let eval_stmt rng obs_map trace stmt =
  match stmt with
  | LetDet (var, expr) ->
      if Env.mem var trace.env || Env.mem var obs_map then
        raise
          (Runtime_error
             (Redefined_variable
                (Printf.sprintf "%s was defined multiple times" var)))
      else { trace with env = Env.add var (eval_expr trace.env expr) trace.env }
  | LetRand (var, dist) -> (
      if Env.mem var trace.env then
        raise
          (Runtime_error
             (Redefined_variable
                (Printf.sprintf "%s was defined multiple times" var)))
      else
        match eval_dist trace.env dist with
        | Any dist -> (
            let k = Distributions.kind dist in
            match Env.find_opt var obs_map with
            | Some v ->
                {
                  env = Env.add var v trace.env;
                  log_weight =
                    trace.log_weight +. Distributions.logpdf dist (of_value v k);
                }
            | None ->
                {
                  trace with
                  env =
                    Env.add var
                      (to_value k (Distributions.sample rng dist))
                      trace.env;
                }))

let eval_obs obs_map obs =
  let var, value = obs in
  if Env.mem var obs_map then
    raise
      (Runtime_error
         (Redefined_variable
            (Printf.sprintf "%s was observed multiple times" var)))
  else Env.add var value obs_map

let eval_program rng prog =
  try
    let obs_map = List.fold_left eval_obs Env.empty prog.observations in
    let trace =
      List.fold_left (eval_stmt rng obs_map)
        { env = Env.empty; log_weight = 0.0 }
        prog.stmts
    in
    Ok trace
  with Runtime_error msg -> Error msg
