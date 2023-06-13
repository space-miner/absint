module Extinct : sig
  type t =
    | PosInf
    | NegInf
    | Int of Z.t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( == ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val toString : t -> string
end = struct
  type t =
    | PosInf
    | NegInf
    | Int of Z.t

  let toString x =
    match x with
    | PosInf -> "PosInf"
    | NegInf -> "NegInf"
    | Int z -> Z.to_string z
  ;;

  let ( == ) t1 t2 =
    match t1, t2 with
    | NegInf, NegInf | PosInf, PosInf -> true
    | NegInf, _ | PosInf, _ | _, NegInf | _, PosInf -> false
    | Int x, Int y -> Z.equal x y
  ;;

  let ( + ) t1 t2 =
    match t1, t2 with
    | NegInf, PosInf | PosInf, NegInf -> failwith "(IDK) adding opposite sign infinities"
    | NegInf, _ | _, NegInf -> NegInf
    | PosInf, _ | _, PosInf -> PosInf
    | Int z1, Int z2 -> Int (Z.add z1 z2)
  ;;

  let ( - ) t1 t2 =
    match t1, t2 with
    | NegInf, NegInf | PosInf, PosInf -> failwith "(IDK) subtracting same sign infinities"
    | NegInf, _ | _, PosInf -> NegInf
    | PosInf, _ | _, NegInf -> PosInf
    | Int z1, Int z2 -> Int (Z.sub z1 z2)
  ;;

  let min t1 t2 : t =
    match t1, t2 with
    | PosInf, PosInf -> PosInf
    | NegInf, _ | _, NegInf -> NegInf
    | Int x, PosInf | PosInf, Int x -> Int x
    | Int x, Int y -> Int (Z.min x y)
  ;;

  let max t1 t2 : t =
    match t1, t2 with
    | PosInf, _ | _, PosInf -> PosInf
    | NegInf, x | x, NegInf -> x
    | Int x, Int y -> Int (Z.max x y)
  ;;
end
