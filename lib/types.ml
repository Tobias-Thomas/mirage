type value = Continuous of float | Discrete of int | Binary of bool
[@@deriving show { with_path = false }]

let float_of_value v =
  match v with
  | Continuous f -> f
  | Discrete i -> float_of_int i
  | Binary true -> 1.0
  | Binary false -> 0.0

type bin_op = Add | Sub | Mul | Div | Gt | Lt | Eq
[@@deriving show { with_path = false }]

type un_op = Neg [@@deriving show { with_path = false }]

type expr =
  | Literal of value
  | Var of string
  | BinOp of bin_op * expr * expr
  | UnOp of un_op * expr
[@@deriving show { with_path = false }]

type dist =
  | PUniform of expr * expr
  | PExponential of expr
  | PBernoulli of expr
  | PNormal of expr * expr
[@@deriving show { with_path = false }]

type stmt = LetDet of string * expr | LetRand of string * dist
[@@deriving show { with_path = false }]

type observation = string * value [@@deriving show { with_path = false }]

type program = { stmts : stmt list; observations : observation list }
[@@deriving show { with_path = false }]
