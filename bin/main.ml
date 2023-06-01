open Stdio
open Astprint

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let p = Parser.prog Lexer.token lexbuf in
    let _ = print_endline "" in
    printProg p 0
  with
  | Lexer.Error msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
