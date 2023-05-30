open Base
open Stdio

let () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.prog Lexer.token lexbuf in
  print_s [%sexp (p : Parser.prog)]
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
