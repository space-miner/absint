
open Stdio
open Syntax


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
    print_endline ("(VAR EXPR :" ^ s ^ ")")
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
  | Seq (_, c1, _, c2) ->
    let _ = indent depth "" in
    let _ = print_endline "(SEQ:" in
    let _ = printCmd c1 (depth+2) in
    let _ = indent (depth+2) "" in
    let _ = printCmd c2 (depth+2) in
    let _ = indent depth "" in
    print_endline ")"
  | Assume (_, c) ->
    let _ = indent depth "" in
    let _ = print_endline "(Assume:" in
    let _ = printCond c (depth+2) in
    let _ = indent depth "" in
    print_endline ")"
  | While (_, c1, c2) ->
    let _ = indent depth "" in
    let _ = print_endline "(While:" in
    let _ = printCond c1 (depth+2) in
    let _ = printCmd c2 (depth+2) in
    let _ = indent depth "" in
    print_endline ")"
  | Choice (_, c1, c2) ->
    let _ = indent depth "" in
    let _ = print_endline "(Choice:" in
    let _ = printCmd c1 (depth+2) in
    let _ = printCmd c2 (depth+2) in
    let _ = indent depth "" in
    print_endline ")"
  | Assign (_, v, e) ->
    let _ = indent depth "" in
    let _ = print_endline "(Assign:" in
    let _ = indent (depth+2) "" in
    let _ = print_endline ("VAR:" ^ v) in
    let _ = printExpr e (depth+2) in
    let _ = indent depth "" in
    print_endline ")"


let printProg prog depth =
  match prog with
  | Prog c -> printCmd c depth


  let () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.prog Lexer.token lexbuf in
  let _ = print_endline "" in
  printProg p 0
;;


(*
let () = 
  let lexbuf = Lexing.from_channel stdin in
    let _ = (Parser.prog Lexer.token lexbuf) 0
  print_line "hello"


try
    let _ = (Parser.prog Lexer.token lexbuf) 0
  with
    | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
    | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!"
                                     (Lexing.lexeme_start lexbuf);;*)
