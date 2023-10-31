open Base
open Syntax

type t = (var, Sign_domain.t) Hashtbl.t

let is_subset t1 t2 =
  Hashtbl.fold
    ~init:true
    ~f:(fun ~key:var ~data:sign1 bool_acc ->
      let sign2 = Option.value (Hashtbl.find t2 var) ~default:Sign_domain.(Bottom) in
      Sign_domain.is_subset sign1 sign2 && bool_acc)
    t1
;;

let rec ( == ) t1 t2 = is_subset t1 t2 && is_subset t2 t1
and ( <> ) t1 t2 = not (t1 == t2)

let join t1 t2 =
  Hashtbl.fold
    ~init:t1
    ~f:(fun ~key:variable ~data:sign acc ->
      match Hashtbl.find t1 variable with
      | None -> acc
      | Some sign' ->
        let sign'' = Sign_domain.join sign sign' in
        let _ = Hashtbl.set acc ~key:variable ~data:sign'' in
        acc)
    t2
;;

let to_string t =
  String.(
    "Memory {\n"
    ^ Hashtbl.fold t ~init:"" ~f:(fun ~key ~data:value str_acc ->
      str_acc ^ key ^ ": " ^ Sign_domain.to_string value ^ "\n")
    ^ "}")
;;
