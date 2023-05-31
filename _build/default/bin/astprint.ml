open Syntax
open Stdio

let rec indent depth res = 
  match depth with
  | 0 -> print_string res
  | _ -> indent (depth-1) (res ^ " ");;


let rec printExpr exp depth =
  match exp with
  | Const c ->
    let _ = indent depth "" in
    print_endline ("(CONST EXPR " ^ (Z.to_string c) ^ ")")  
  | Var s -> 
    let _ = indent depth "" in
    print_endline ("(VAR EXPR " ^ s ^ ")")
  | Binop (_, e1, e2) ->
    let _ = indent depth "" in
    let _ = print_endline ("(BIN EXPR ") in
    let _ = printExpr e1 (depth+2) in
    let _ = printExpr e2 (depth+2) in
    let _ = indent depth "" in
    print_endline ")" 

(*
let printBinop b depth =
  let _ = indent depth "" in
  match b with 
    | Add -> print_endline "ADD"
    | Sub -> print_endline "SUB"
*)
let printCmpop b depth =
  let _ = indent depth "" in
  match b with 
    | Less -> print_endline "LESS"
    | Equal -> print_endline "EQ"

    let printCond (Cmp (b, e1, e2)) depth = 
      let _ = indent depth "" in
      let _ = print_endline "(COND" in
      let _ = printCmpop b (depth+2) in
      let _ = printExpr e1 (depth+2) in
      let _ = printExpr e2 (depth+2) in
      let _ = indent depth "" in
      print_endline ")"
    
    let rec printCmd cmd depth =
      match cmd with
      | Seq (l1, c1, l2, c2) ->
        let _ = indent depth "" in
        let _ = print_endline ("(SEQ l" ^ (string_of_int l1)) in
        let _ = printCmd c1 (depth+2) in
        let _ = indent (depth+2) "" in
        let _ = print_endline ("l" ^ (string_of_int l2)) in
        let _ = printCmd c2 (depth+2) in
        let _ = indent depth "" in
        print_endline ")"
      | Assume (l, c) ->
        let _ = indent depth "" in
        let _ = print_endline ("(ASSUME l" ^ (string_of_int l)) in
        let _ = printCond c (depth+2) in
        let _ = indent depth "" in
        print_endline ")"
      | While (l, c1, c2) ->
        let _ = indent depth "" in
        let _ = print_endline ("(WHILE l" ^ (string_of_int l)) in
        let _ = printCond c1 (depth+2) in
        let _ = printCmd c2 (depth+2) in
        let _ = indent depth "" in
        print_endline ")"
      | Choice (l, c1, c2) ->
        let _ = indent depth "" in
        let _ = print_endline ("(CHOICE l" ^ (string_of_int l)) in
        let _ = printCmd c1 (depth+2) in
        let _ = printCmd c2 (depth+2) in
        let _ = indent depth "" in
        print_endline ")"
      | Assign (_, v, e) ->
        let _ = indent depth "" in
        let _ = print_endline "(Assign:" in
        let _ = indent (depth+2) "" in
        let _ = print_endline ("VAR" ^ v) in
        let _ = printExpr e (depth+2) in
        let _ = indent depth "" in
        print_endline ")"
    
    
    let printProg prog depth =
      match prog with
      | Prog c -> printCmd c depth