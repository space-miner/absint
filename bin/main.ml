open Stdio
open Astprint
open Bigint

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let p = Parser.prog Lexer.token lexbuf in
    let _ = print_endline "" in
<<<<<<< HEAD
    let _ = BigInt.PosInf in
=======
>>>>>>> 0fe990fa5d80712c9588deb482d51db37baba64f
    print_prog p 0
  with
  | Lexer.Error msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
