open Base
open Absint
open Interval
open Astprint

let () =
  let lexbuf = Lexing.from_channel Stdio.stdin in
  try
    let p = Parser.prog Lexer.token lexbuf in
    let mem = Hashtbl.create (module String) in
    let glbl = Absint.absint_iter p mem in
    let _ =
      Hashtbl.iteri glbl ~f:(fun ~key:lbl ~data:mem ->
        let _ = Stdio.printf "label%d: \n" lbl in
        Hashtbl.iteri mem ~f:(fun ~key:var ~data:d ->
          let _ = Stdio.printf "%s: " var in
          let s = Interval.to_string d in
          Stdio.printf "%s\n" s))
    in
    let i = Interval.meet (Some (Int (Z.of_int 0), Int (Z.of_int 5))) (Some (Int (Z.of_int 3) , PosInf)) in
    let _ = Stdio.printf "%s\n" (Interval.to_string i) in 
    print_prog p 0

  with
  | Lexer.Error msg -> Stdio.printf "%s%!" msg
  | Parser.Error ->
    Stdio.printf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
