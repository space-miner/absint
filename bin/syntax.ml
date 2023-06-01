type label = int
type const = Z.t
type var = string

type binop =
  | Add
  | Sub
[@@deriving sexp]

type cmpop =
  | Less
  | Equal
[@@deriving sexp]

type expr =
  | Const of const
  | Var of string
  | Binop of binop * expr * expr

type cond = Cmp of cmpop * expr * expr

type cmd =
  | Seq of label * cmd * cmd
  | Assume of label * cond
  | While of label * cond * cmd
  | Choice of label * cmd * cmd
  | Assign of label * var * expr

type prog = Prog of cmd * label
