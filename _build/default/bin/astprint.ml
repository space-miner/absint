open Stdio
open Syntax

let rec indent depth res =
  match depth with
  | 0 -> print_string res
  | _ -> indent (depth - 1) (res ^ " ")
;;

let rec print_expr exp depth =
  match exp with
  | Const c ->
    let _ = indent depth "" in
    print_endline ("(CONST EXPR " ^ Z.to_string c ^ ")")
  | Var s ->
    let _ = indent depth "" in
    print_endline ("(VAR EXPR " ^ s ^ ")")
  | Binop (_, e1, e2) ->
    let _ = indent depth "" in
    let _ = print_endline "(BIN EXPR " in
    let _ = print_expr e1 (depth + 2) in
    let _ = print_expr e2 (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
;;

(*
let printBinop b depth =
  let _ = indent depth "" in
  match b with 
    | Add -> print_endline "ADD"
    | Sub -> print_endline "SUB"
*)
let print_cmpop b depth =
  let _ = indent depth "" in
  match b with
  | Less -> print_endline "LESS"
  | Equal -> print_endline "EQ"
;;

let print_cond (Cmp (b, e1, e2)) depth =
  let _ = indent depth "" in
  let _ = print_endline "(COND" in
  let _ = print_cmpop b (depth + 2) in
  let _ = print_expr e1 (depth + 2) in
  let _ = print_expr e2 (depth + 2) in
  let _ = indent depth "" in
  print_endline ")"
;;

let rec print_cmd cmd depth =
  match cmd with
  | Seq (l1, c1, c2) ->
    let _ = indent depth "" in
    let _ = print_endline ("(SEQ l" ^ string_of_int l1) in
    let _ = print_cmd c1 (depth + 2) in
    let _ = print_cmd c2 (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
  | Assume (l, c) ->
    let _ = indent depth "" in
    let _ = print_endline ("(ASSUME l" ^ string_of_int l) in
    let _ = print_cond c (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
  | While (l, c1, c2) ->
    let _ = indent depth "" in
    let _ = print_endline ("(WHILE l" ^ string_of_int l) in
    let _ = print_cond c1 (depth + 2) in
    let _ = print_cmd c2 (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
  | Choice (l, c1, c2) ->
    let _ = indent depth "" in
    let _ = print_endline ("(CHOICE l" ^ string_of_int l) in
    let _ = print_cmd c1 (depth + 2) in
    let _ = print_cmd c2 (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
  | Assign (l, v, e) ->
    let _ = indent depth "" in
    let _ = print_endline ("(ASSIGN l" ^ string_of_int l) in
    let _ = indent (depth + 2) "" in
    let _ = print_endline ("VAR " ^ v) in
    let _ = print_expr e (depth + 2) in
    let _ = indent depth "" in
    print_endline ")"
;;

let print_prog prog depth =
  match prog with
  | Prog (c, l) ->
    let _ = print_cmd c depth in
    let _ = indent depth "" in
    print_endline ("l" ^ string_of_int l)
;;
