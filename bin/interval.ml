open Bigint
open Syntax

module Interval : sig

  type t
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val join : t -> t -> t
  val binop : binop -> BigInt.t -> BigInt.t -> BigInt.t

end = struct
  
  type t = (BigInt.t * BigInt.t) option

  let join t t' =
    match t, t' with
    | None, None -> None
    | None, t | t, None -> t
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.min lo1 lo2, BigInt.max hi1 hi2)
  ;;

  let ( - ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( - ) lo1 hi2, BigInt.( - ) hi1 lo2)
  ;;

  let ( + ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( + ) lo1 lo2, BigInt.( + ) hi1 hi2)
  ;;

  let binop op t t' =
    match op with
    | Add -> BigInt.( + ) t t'
    | Sub -> BigInt.( - ) t t'
  ;;
end
