open Stdio
open Astprint
open Ai

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let (Prog (c, l)) = Parser.prog Lexer.token lexbuf in
    let _ = print_endline "" in
    let _ = printProg (Prog (c, l)) 0 in 
    let c = findCmd c 1 in
    match c with 
    | None -> failwith "err"
    | Some c -> printCmd c 0
  with
  | Lexer.Error msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
