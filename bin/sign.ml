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

let to_string t = Base.Sexp.to_string [%sexp (t : t)]

let meet t1 t2 =
  match t1, t2 with
  | _, Top -> t1
  | Top, _ -> t2
  | Bottom, _ | _, Bottom -> Bottom
  | Positive, NonNegative | NonNegative, Positive -> Positive
  | Negative, NonPositive | NonPositive, Negative -> Negative
  | NonPositive, NonNegative | NonNegative, NonPositive -> Zero
  | Positive, Positive -> Positive
  | Negative, Negative -> Negative
  | Zero, Zero -> Zero
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
;;

let join t1 t2 =
  match t1, t2 with
  | _, Bottom -> t1
  | Bottom, _ -> t2
  | Top, _ | _, Top -> Top
  | Negative, Zero | Zero, Negative -> NonPositive
  | NonPositive, Zero | Zero, NonPositive -> NonPositive
  | Positive, Zero | Zero, Positive -> NonPositive
  | NonNegative, Zero | Zero, NonNegative -> NonPositive
  | Positive, Positive -> Positive
  | Negative, Negative -> Negative
  | Zero, Zero -> Zero
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
;;

let ( - ) t1 t2 =
  match t1,t2 with
  | Positive, Negative | Positive, NonPositive -> Positive
  | Negative, Positive | Negative, NonNegative -> Negative
  | _, Zero -> t1
  | Zero, Positive -> Negative
  | Zero, Negative -> Positive
  | Zero, NonPositive -> NonNegative
  | Zero, NonNegative -> NonPositive
  | _ -> Bottom

let ( + ) t1 t2 =
  | _, Zero -> t1
  | Zero, _ -> t2
  | Positive, Positive -> Positive
  | Negative, Negative -> Negative`
  | NonPositive, NonPositive -> NonPositive
  | NonNegative, NonNegative -> NonNegative
  | Positive, NonNegative | NonNegative, Positive -> Positive
  | Negative, NonPositive | NonPositive, Negative -> Negative
  | _ -> Bottom
