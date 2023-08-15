open Syntax
open Base

val absint_expression : expr -> Memory.t -> Interval.t
val absint_command : cmd -> (label, Memory.t) Hashtbl.t -> label -> label list
val absint_iter : prog -> Memory.t -> (label, (var, Interval.t) Hashtbl.t) Hashtbl.t

val absint_iter_loop
  :  label Stack.t
  -> (label, Memory.t) Hashtbl.t
  -> cmd
  -> label
  -> (label, Memory.t) Hashtbl.t
