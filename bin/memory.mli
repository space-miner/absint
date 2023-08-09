open Syntax
open Base

type t = (var, Interval.t) Hashtbl.t

val ( == ) : t -> t -> bool
val ( <> ) : t -> t -> bool
val join : t -> t -> t
val to_string : t -> string
