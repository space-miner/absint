open Parser
open Lexer

(*
let lexbuf = Lexing.from_channel stdin in
  try
    let _ = (Parser.prog Lexer.token lexbuf) 0
  with
  | Lexer.Error msg ->
    Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!"
                                     (Lexing.lexeme_start lexbuf);;*)
