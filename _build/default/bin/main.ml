open Base
open Stdio

let () = print_s [%sexp (Z.to_string Z.zero : string)]
