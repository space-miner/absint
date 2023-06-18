open Base
open Absint
open Astprint
open Util


let () =
  let lexbuf = Lexing.from_channel Stdio.stdin in
  try
    let p = Parser.prog Lexer.token lexbuf in
    let mem = Hashtbl.create (module String) in
    let global = Absint.absint_iter p mem in
    let _ = Util.print_global_mem global in
    print_prog p 0
  with
  | Lexer.Error msg -> Stdio.printf "%s%!" msg
  | Parser.Error ->
    Stdio.printf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
