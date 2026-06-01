type value = Continuous of float | Discrete of int | Binary of bool
type op = Add | Sub | Mul | Div | Gt | Lt | Eq

type expr =
  | Literal of value
  | Var of string
  | Dist of Distributions.distribution
  | BinOp of op * expr * expr

and stmt =
  | LetDet of string * expr
  | LetRand of string * expr
  | Observe of expr
  | Sample of string

type program = stmt list
