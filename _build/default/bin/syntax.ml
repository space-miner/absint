type label = int
type const = Z.t
type var = string


type binop =
  | Add
  | Sub


type cmpop =
  | Lt
  | Eq

type expr =
  | EConst of const
  | EVar of string
  | EBinop of binop * expr * expr


type cond = CmpVarConst of cmpop * expr * expr 

type cmd =
  | CSeq of label * cmd * label * cmd
  | CAssume of label * cond
  | CWhile of label * cond * cmd
  | CChoice of label * cmd * cmd
  | CAssign of label * var * expr


type prog = Prog of cmd

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"


let string_of_cmpop = function
  | Lt -> "<"
  | Eq -> "="


let rec string_of_expr = function
  | EConst c -> Z.to_string c
  | EVar v -> v
  | EBinop (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"


let string_of_cond = function
  | CmpVarConst (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_cmpop op ^ " " ^ string_of_expr e2 ^ ")"


let rec string_of_cmd = function
  | CSeq (_, c1, _, c2) ->
    string_of_cmd c1 ^ ";\n" ^ string_of_cmd c2
  | CAssume (l, cond) ->
    "assume label " ^ string_of_int l ^ ": " ^ string_of_cond cond
  | CWhile (l, cond, body) ->
    "while label " ^ string_of_int l ^ ": " ^ string_of_cond cond ^ " do\n" ^
    "  " ^ string_of_cmd body ^ "\n" ^
    "done"
  | CChoice (l, c1, c2) ->  
    "choice label " ^ string_of_int l ^ ":\n" ^
    "  " ^ string_of_cmd c1 ^ "\n" ^
    "  " ^ string_of_cmd c2
  | CAssign (l, v, e) ->
    "assign label " ^ string_of_int l ^ ":\n" ^
    "  " ^ v ^ "\n" ^
    "  " ^ string_of_expr e
    


let string_of_prog (Prog cmd) = string_of_cmd cmd


let print_prog prog =
  let str = string_of_prog prog in
  print_endline str
