open Types

type error =
  | Unbound_variable of string
  | Type_error of string
  | Invalid_distribution of string

exception Runtime_error of error

module Env = Map.Make (String)

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

let eval_dist rng env dist_name dist_args =
  let eval_args =
    List.map (fun a -> float_of_value (eval_expr env a)) dist_args
  in
  match Distributions.of_name dist_name eval_args with
  | Ok d -> Samplers.sample rng d
  | Error msg -> raise (Runtime_error (Invalid_distribution msg))

let eval_stmt rng env stmt =
  match stmt with
  | LetDet (var, expr) -> Env.add var (eval_expr env expr) env
  | LetRand (var, (name, args)) -> Env.add var (eval_dist rng env name args) env
  | _ -> raise (Runtime_error (Type_error "observe is not implemented yet"))

let eval_program rng prog =
  try Ok (List.fold_left (eval_stmt rng) Env.empty prog)
  with Runtime_error msg -> Error msg
