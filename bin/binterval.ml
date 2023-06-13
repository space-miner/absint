open Extinct

module Binterval : sig
  type t = (Extinct.t * Extinct.t) option

  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val join : t -> t -> t
  val binop : Syntax.binop -> Extinct.t -> Extinct.t -> Extinct.t
  val is_subset : t -> t -> bool
  val toString : t -> string
end = struct
  type t = (Extinct.t * Extinct.t) option

  let is_subset (t1 : t) (t2 : t) =
    match t1, t2 with
    | None, _ -> true
    | _, None -> false
    | Some (lo1, hi1), Some (lo2, hi2) ->
      Extinct.( == ) (Extinct.min lo1 lo2) lo2 && Extinct.( == ) (Extinct.max hi1 hi2) hi2
  ;;

  let toString t =
    match t with
    | None -> ""
    | Some (e1, e2) -> "[" ^ Extinct.toString e1 ^ ", " ^ Extinct.toString e2 ^ "]"
  ;;

  let join t t' =
    match t, t' with
    | None, None -> None
    | None, t | t, None -> t
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (Extinct.min lo1 lo2, Extinct.max hi1 hi2)
  ;;

  let ( - ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) ->
      Some (Extinct.( - ) lo1 hi2, Extinct.( - ) hi1 lo2)
  ;;

  let ( + ) t t' =
    match t, t' with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) ->
      Some (Extinct.( + ) lo1 lo2, Extinct.( + ) hi1 hi2)
  ;;

  let binop op t t' =
    match op with
    | Syntax.Add -> Extinct.( + ) t t'
    | Syntax.Sub -> Extinct.( - ) t t'
  ;;
end
