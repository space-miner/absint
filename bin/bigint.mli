type t =  
| PosInf
| NegInf
| Int of Z.t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( == ) : t -> t -> bool
val ( < ) : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val to_string : t -> string

