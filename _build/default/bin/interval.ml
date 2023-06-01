open Syntax

module BigInt = struct
  type t =
    | PosInf
    | NegInf
    | Int of Z.t

  let ( + ) t1 t2 =
    match t1, t2 with
    | NegInf, PosInf | PosInf, NegInf -> failwith "(IDK) adding pos/neg infinities"
    | NegInf, _ | _, NegInf -> NegInf
    | PosInf, _ | _, PosInf -> PosInf
    | Int z1, Int z2 -> Int (Z.add z1 z2)
  ;;

  let ( - ) t1 t2 =
    match t1, t2 with
    | NegInf, NegInf | PosInf, PosInf ->
      failwith "(IDK) subtracting same sign infintities"
    | NegInf, _ | _, PosInf -> NegInf
    | PosInf, _ | _, NegInf -> PosInf
    | Int z1, Int z2 -> Int (Z.sub z1 z2)
  ;;

  let min t1 t2 : t =
    match t1, t2 with
    | NegInf, _ -> NegInf
    | _, NegInf -> NegInf
    | PosInf, PosInf -> PosInf
    | Int x, PosInf -> Int x
    | PosInf, Int x -> Int x
    | Int x, Int y -> Int (Z.min x y)
  ;;

  let max t1 t2 : t =
    match t1, t2 with
    | PosInf, _ | _, PosInf -> PosInf
    | NegInf, x | x, NegInf -> x
    | Int x, Int y -> Int (Z.max x y)
  ;;
end

type interval = (BigInt.t * BigInt.t) option

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

type mem = interval StringMap.t option
type global = mem IntMap.t

(*i1 \subset i2*)
let incl i1 _ =
  match i1 with
  | Some _ -> false
  | None -> true
;;

let intervalSub i1 i2 =
  match i1, i2 with
  | None, _ | _, None -> failwith "err"
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( - ) lo1 hi2, BigInt.( - ) hi1 lo2)
;;

let intervalAdd i1 i2 =
  match i1, i2 with
  | None, _ | _, None -> failwith "err"
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( + ) lo1 lo2, BigInt.( + ) hi1 hi2)
;;

let intervalBinop binop i1 i2 =
  match binop with
  | Add -> intervalAdd i1 i2
  | Sub -> intervalSub i1 i2
;;
