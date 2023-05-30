type label = int
type const = Z.t
type var = int

type arith_op =
  | Plus
  | Minus

type cmp_op =
  | LessThan
  | Equal

type expr =
  | Const of const
  | Var of var
  | Binop of arith_op * expr * expr

type cond = Binop of cmp_op * var * const

type cmd =
  | Assume of label * cond
  | While of label * cond * cmd
  | Seq of label * cmd * cmd
  | EOF
