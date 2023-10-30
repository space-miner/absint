type t =
  | Positive
  | Negative
  | Zero
  | NonPositive
  | NonNegative
  | Bottom
  | Top
[@@deriving sexp]

let not t =
  match t with
  | Positive -> NonPositive
  | NonPositive -> Positive
  | Negative -> NonNegative
  | NonNegative -> Negative
  | _ -> Bottom
;;

let of_const c =
  if Z.gt c Z.zero then Positive else if Z.lt c Z.zero then Negative else Zero
;;

let to_string t = Base.Sexp.to_string [%sexp (t : t)]

let meet t1 t2 =
  match t1, t2 with
  | _, Top -> t1
  | Top, _ -> t2
  | Positive, Positive | Positive, NonNegative | NonNegative, Positive -> Positive
  | Negative, Negative | Negative, NonPositive | NonPositive, Negative -> Negative
  | NonPositive, NonNegative
  | NonNegative, NonPositive
  | NonNegative, Zero
  | NonPositive, Zero
  | Zero, NonNegative
  | Zero, NonPositive
  | Zero, Zero -> Zero
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
  | _ -> Bottom
;;

let join t1 t2 =
  match t1, t2 with
  | _, Bottom -> t1
  | Bottom, _ -> t2
  | Top, _ | _, Top -> Top
  | Negative, Zero | Zero, Negative -> NonPositive
  | NonPositive, Zero | Zero, NonPositive -> NonPositive
  | Positive, Zero | Zero, Positive -> NonNegative
  | NonNegative, Zero | Zero, NonNegative -> NonNegative
  | Positive, Positive -> Positive
  | Negative, Negative -> Negative
  | Zero, Zero -> Zero
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
  | Negative, Positive -> Bottom (* this has a hole (Zero) *)
  | Negative, NonNegative -> Top
  | Negative, NonPositive -> Negative
  | NonPositive, Negative -> Negative
  | NonPositive, Positive -> Top
  | NonPositive, NonNegative -> Top
  | Positive, Negative -> Bottom (* this has a hole (Zero) *)
  | Positive, NonPositive -> Top
  | Positive, NonNegative -> Positive
  | NonNegative, Positive -> Positive
  | NonNegative, Negative -> Top
  | NonNegative, NonPositive -> Top
;;

let ( - ) t1 t2 =
  match t1, t2 with
  | Positive, Negative | Positive, NonPositive -> Positive
  | Negative, Positive | Negative, NonNegative -> Negative
  | _, Zero -> t1
  | Zero, Positive -> Negative
  | Zero, Negative -> Positive
  | Zero, NonPositive -> NonNegative
  | Zero, NonNegative -> NonPositive
  | _ -> Bottom
;;

let ( + ) t1 t2 =
  match t1, t2 with
  | _, Zero -> t1
  | Zero, _ -> t2
  | Positive, Positive -> Positive
  | Negative, Negative -> Negative
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
  | Positive, NonNegative | NonNegative, Positive -> Positive
  | Negative, NonPositive | NonPositive, Negative -> Negative
  | _ -> Bottom
;;

let is_subset t1 t2 =
  match t1, t2 with
  | _, Top -> true
  | Bottom, _ -> true
  | Positive, Positive -> true
  | Negative, Negative -> true
  | NonPositive, NonPositive -> true
  | NonNegative, NonNegative -> true
  | Negative, NonPositive | NonPositive, Negative -> true
  | _ -> false
;;
