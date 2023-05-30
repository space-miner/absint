type label = int
type const = int
type var = string

type binop =
  | Add
  | Sub
[@@deriving sexp]

type cmpop =
  | Lt
  | Eq
[@@deriving sexp]

type expr =
  | EConst of const
  | EVar of var
  | EBinop of binop * expr * expr
[@@deriving sexp]

type cond = CmpVarConst of cmpop * expr * expr [@@deriving sexp]

type cmd =
  | CSeq of label * cmd * label * cmd
  | CAssume of label * cond
  | CWhile of label * cond * cmd
  | CChoice of label * cmd * cmd
  | CSkip
[@@deriving sexp]

type prog = Prog of cmd [@@deriving sexp]
