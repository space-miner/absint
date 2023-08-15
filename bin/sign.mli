type t =
  | Positive
  | Negative
  | Zero
  | NonPositive
  | NonNegative
  | Bottom
  | Top

val not : t -> t
val to_string : t -> string
val meet : t -> t -> t
val join : t -> t -> t
val ( - ) : t -> t -> t
val ( + ) : t -> t -> t
