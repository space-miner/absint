open Syntax
open Base

val absint_expression : expr -> Memory_sign.t -> Sign_domain.t
val absint_command : cmd -> (label, Memory_sign.t) Hashtbl.t -> label -> label list

val absint_iter
  :  prog
  -> Memory_sign.t
  -> (label, (var, Sign_domain.t) Hashtbl.t) Hashtbl.t

val absint_iter_loop
  :  label Stack.t
  -> (label, Memory_sign.t) Hashtbl.t
  -> cmd
  -> label
  -> (label, Memory_sign.t) Hashtbl.t
