open Base
open Syntax

type t = (var, Interval.t) Hashtbl.t

let is_subset t1 t2 =
  Hashtbl.fold
    ~init:true
    ~f:(fun ~key:var ~data:interval1 bool_acc ->
      let interval2 = Option.value (Hashtbl.find t2 var) ~default:Interval.Bottom in
      Interval.is_subset interval1 interval2 && bool_acc)
    t1
;;

let rec ( == ) t1 t2 = is_subset t1 t2 && is_subset t2 t1
and ( <> ) t1 t2 = not (t1 == t2)

let join t1 t2 =
  Hashtbl.fold
    ~init:t1
    ~f:(fun ~key:variable ~data:interval acc ->
      match Hashtbl.find t1 variable with
      | None -> acc
      | Some interval' ->
        let interval'' = Interval.join interval interval' in
        let _ = Hashtbl.set acc ~key:variable ~data:interval'' in
        acc)
    t2
;;

let to_string t =
  String.(
    "Memory {\n"
    ^ Hashtbl.fold t ~init:"" ~f:(fun ~key ~data:value str_acc ->
      str_acc ^ key ^ ": " ^ Interval.to_string value ^ "\n")
    ^ "}")
;;
