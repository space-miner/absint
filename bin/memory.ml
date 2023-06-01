open Base
open Interval

type variable = string

module Memory = struct
  type t = (variable, Interval.t) Hashtbl.t

  let join t t' =
    Hashtbl.fold t' ~init:t ~f:(fun variable interval acc ->
      match Hashtbl.find variable init with
      | None -> acc
      | Some interval' ->
        let interval'' = Interval.join interval interval' in
        let _ = Hashtbl.set acc ~key:variable ~data:interval'' in
        acc)
  ;;
end
