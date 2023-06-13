open Bigint

module Interval : sig
  type t = (BigInt.t * BigInt.t) option

  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val join : t -> t -> t
  val binop : Syntax.binop -> BigInt.t -> BigInt.t -> BigInt.t
  val is_subset : t -> t -> bool
  val to_string : t -> string
end = struct
  type t = (BigInt.t * BigInt.t) option

  let is_subset (t1 : t) (t2 : t) =
    match t1, t2 with
    | None, _ -> true
    | _, None -> false
    | Some (lo1, hi1), Some (lo2, hi2) ->
      BigInt.( == ) (BigInt.min lo1 lo2) lo2 && BigInt.( == ) (BigInt.max hi1 hi2) hi2
  ;;

  let to_string t =
    match t with
    | None -> ""
    | Some (e1, e2) -> "[" ^ BigInt.to_string e1 ^ ", " ^ BigInt.to_string e2 ^ "]"
  ;;

  let join t t' =
    match t, t' with
    | None, None -> None
    | None, t | t, None -> t
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.min lo1 lo2, BigInt.max hi1 hi2)
  ;;

  let ( - ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) ->
      Some (BigInt.( - ) lo1 hi2, BigInt.( - ) hi1 lo2)
  ;;

  let ( + ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) ->
      Some (BigInt.( + ) lo1 lo2, BigInt.( + ) hi1 hi2)
  ;;

  let binop op t t' =
    match op with
    | Syntax.Add -> BigInt.( + ) t t'
    | Syntax.Sub -> BigInt.( - ) t t'
  ;;
end
