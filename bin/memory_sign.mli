open Syntax
open Base

type t = (var, Sign_domain.t) Hashtbl.t

val ( == ) : t -> t -> bool
val ( <> ) : t -> t -> bool
val join : t -> t -> t
val to_string : t -> string
val is_subset : t -> t -> bool
