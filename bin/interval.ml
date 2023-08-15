type t =
  | Interval of (Bigint.t * Bigint.t)
  | Bottom

let one = Interval (Bigint.one, Bigint.one)

let not t =
  Bigint.(
    match t with
    | Interval (Int _lo, Int _hi) -> Interval (NegInf, PosInf)
    | Interval (NegInf, Int hi) -> Interval (Int hi + Int Z.one, PosInf)
    | Interval (Int lo, PosInf) -> Interval (NegInf, Int lo - Int Z.one)
    | _ -> failwith "not valid interval -- i.e intervals of the form [x, -inf] | [inf, x]")
;;

let is_subset t1 t2 =
  match t1, t2 with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Interval (lo1, hi1), Interval (lo2, hi2) ->
    Bigint.(min lo1 lo2 == lo2 && max hi1 hi2 == hi2)
;;

let to_string t =
  match t with
  | Bottom -> "[]"
  | Interval (e1, e2) -> "[" ^ Bigint.to_string e1 ^ ", " ^ Bigint.to_string e2 ^ "]"
;;

let meet t1 t2 =
  match t1, t2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Interval (lo1, hi1), Interval (lo2, hi2) ->
    if Bigint.( < ) hi1 lo2 || Bigint.( < ) hi2 lo1
    then Bottom
    else Interval (Bigint.max lo1 lo2, Bigint.min hi1 hi2)
;;

let join t1 t2 =
  match t1, t2 with
  | Bottom, Bottom -> Bottom
  | Bottom, t | t, Bottom -> t
  | Interval (lo1, hi1), Interval (lo2, hi2) ->
    Interval (Bigint.min lo1 lo2, Bigint.max hi1 hi2)
;;

let ( - ) t1 t2 =
  match t1, t2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Interval (lo1, hi1), Interval (lo2, hi2) ->
    Interval (Bigint.( - ) lo1 hi2, Bigint.( - ) hi1 lo2)
;;

let ( + ) t1 t2 =
  match t1, t2 with
  | Bottom, _ | _, Bottom -> Bottom
  | Interval (lo1, hi1), Interval (lo2, hi2) ->
    Interval (Bigint.( + ) lo1 lo2, Bigint.( + ) hi1 hi2)
;;

let binop op t1 t2 =
  match op with
  | Syntax.Add -> Bigint.( + ) t1 t2
  | Syntax.Sub -> Bigint.( - ) t1 t2
;;
