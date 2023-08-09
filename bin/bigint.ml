type t =
  | PosInf
  | NegInf
  | Int of Z.t

let ( < ) t t' =
  match t, t' with
  | PosInf, _ | _, NegInf -> false
  | NegInf, _ | _, PosInf -> true
  | Int x1, Int x2 -> x1 < x2
;;

let to_string x =
  match x with
  | PosInf -> "PosInf"
  | NegInf -> "NegInf"
  | Int z -> Z.to_string z
;;

let ( == ) t t' =
  match t, t' with
  | NegInf, NegInf | PosInf, PosInf -> true
  | NegInf, _ | PosInf, _ | _, NegInf | _, PosInf -> false
  | Int x, Int y -> Z.equal x y
;;

let ( + ) t t' =
  match t, t' with
  | NegInf, PosInf | PosInf, NegInf -> failwith "(IDK) adding opposite sign infinities"
  | NegInf, _ | _, NegInf -> NegInf
  | PosInf, _ | _, PosInf -> PosInf
  | Int z1, Int z2 -> Int (Z.add z1 z2)
;;

let ( - ) t t' =
  match t, t' with
  | NegInf, NegInf | PosInf, PosInf -> failwith "(IDK) subtracting same sign infinities"
  | NegInf, _ | _, PosInf -> NegInf
  | PosInf, _ | _, NegInf -> PosInf
  | Int z1, Int z2 -> Int (Z.sub z1 z2)
;;

let min t t' =
  match t, t' with
  | PosInf, PosInf -> PosInf
  | NegInf, _ | _, NegInf -> NegInf
  | Int x, PosInf | PosInf, Int x -> Int x
  | Int x, Int y -> Int (Z.min x y)
;;

let max t t' =
  match t, t' with
  | PosInf, _ | _, PosInf -> PosInf
  | NegInf, x | x, NegInf -> x
  | Int x, Int y -> Int (Z.max x y)
;;
