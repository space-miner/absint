type t = (Bigint.t * Bigint.t) option

let not t =
  Bigint.(
    match t with
    | None -> Some (NegInf, PosInf)
    | Some (Int _lo, Int _hi) -> Some (NegInf, PosInf)
    | Some (NegInf, Int hi) -> Some (Int hi + Int Z.one, PosInf)
    | Some (Int lo, PosInf) -> Some (NegInf, Int lo - Int Z.one)
    | Some (NegInf, PosInf) -> None
    | _ -> failwith "not valid interval -- i.e intervals of the form [x, -inf] | [inf, x]")
;;

let is_subset t t2 =
  match t, t2 with
  | None, _ -> true
  | _, None -> false
  | Some (lo1, hi1), Some (lo2, hi2) -> Bigint.(min lo1 lo2 == lo2 && max hi1 hi2 == hi2)
;;

let to_string t =
  match t with
  | None -> ""
  | Some (e1, e2) -> "[" ^ Bigint.to_string e1 ^ ", " ^ Bigint.to_string e2 ^ "]"
;;

let meet t1 t2 =
  match t1, t2 with
  | None, _ | _, None -> None
  | Some (lo1, hi1), Some (lo2, hi2) ->
    if Bigint.( < ) hi1 lo2 || Bigint.( < ) hi2 lo1
    then None
    else Some (Bigint.max lo1 lo2, Bigint.min hi1 hi2)
;;

let join t1 t2 =
  match t1, t2 with
  | None, None -> None
  | None, t | t, None -> t
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (Bigint.min lo1 lo2, Bigint.max hi1 hi2)
;;

let ( - ) t1 t2 =
  match t1, t2 with
  | None, _ | _, None -> failwith "err"
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (Bigint.( - ) lo1 hi2, Bigint.( - ) hi1 lo2)
;;

let ( + ) t1 t2 =
  match t1, t2 with
  | None, _ | _, None -> failwith "err"
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (Bigint.( + ) lo1 lo2, Bigint.( + ) hi1 hi2)
;;

let binop op t1 t2 =
  match op with
  | Syntax.Add -> Bigint.( + ) t1 t2
  | Syntax.Sub -> Bigint.( - ) t1 t2
;;
