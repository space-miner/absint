open Base
open Bigint
open Syntax

module Interval = struct
  type t = (BigInt.t * BigInt.t) option

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
