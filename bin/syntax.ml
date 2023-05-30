type label = int 
type const = Z.t 
type var = string 


type binop =
  | Add
  | Sub


type cmpop =
  | Less
  | Equal

type expr =
  | Const of const
  | Var of string
  | Binop of binop * expr * expr


type cond = Cmp of cmpop * expr * expr 

type cmd =
  | Seq of label * cmd * label * cmd
  | Assume of label * cond
  | While of label * cond * cmd
  | Choice of label * cmd * cmd
  | Assign of label * var * expr


type prog = Prog of cmd

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"


let string_of_cmpop = function
  | Less -> "<"
  | Equal -> "="


let rec string_of_expr = function
  | Const c -> Z.to_string c
  | Var v -> v
  | Binop (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"


let string_of_cond = function
  | Cmp (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_cmpop op ^ " " ^ string_of_expr e2 ^ ")"


let rec string_of_cmd = function
  | Seq (_, c1, _, c2) ->
    "(SEQ" ^ string_of_cmd c1 ^ ";\n" ^ string_of_cmd c2 ^ ")"
  | Assume (l, cond) ->
    "(ASSUME " ^ string_of_int l ^ ": " ^ string_of_cond cond ^ ")"
  | While (l, cond, body) ->
    "(WHILE" ^ string_of_int l ^ ": " ^ string_of_cond cond ^ " do\n" ^
    "  " ^ string_of_cmd body ^ ")\n" 
  | Choice (l, c1, c2) ->  
    "(CHOICE " ^ string_of_int l ^ ":\n" ^
    "  " ^ string_of_cmd c1 ^ "\n" ^
    "  " ^ string_of_cmd c2 ^ ")"
  | Assign (l, v, e) ->
    "(ASSIGN " ^ string_of_int l ^ ":\n" ^
    "  " ^ v ^ "\n" ^
    "  " ^ string_of_expr e ^ ")"
    


let string_of_prog (Prog cmd) = string_of_cmd cmd


let print_prog prog =
  let str = string_of_prog prog in
  print_endline str




