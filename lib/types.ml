type value = Continuous of float | Discrete of int | Binary of bool
[@@deriving show { with_path = false }]

type bin_op = Add | Sub | Mul | Div | Gt | Lt | Eq
[@@deriving show { with_path = false }]

type un_op = Neg [@@deriving show { with_path = false }]

type expr =
  | Literal of value
  | Var of string
  | Dist of string * expr list
  | BinOp of bin_op * expr * expr
  | UnOp of un_op * expr
[@@deriving show { with_path = false }]

and stmt =
  | LetDet of string * expr
  | LetRand of string * expr
  | Observe of expr
  | Sample of string
[@@deriving show { with_path = false }]

type program = stmt list [@@deriving show { with_path = false }]
