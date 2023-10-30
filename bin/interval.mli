type t =
  | Interval of (Bigint.t * Bigint.t)
  | Bottom

val ( - ) : t -> t -> t
val ( + ) : t -> t -> t
val one : t
val of_const : Z.t -> t
val not : t -> t
val join : t -> t -> t
val meet : t -> t -> t
val binop : Syntax.binop -> Bigint.t -> Bigint.t -> Bigint.t
val is_subset : t -> t -> bool
val to_string : t -> string
