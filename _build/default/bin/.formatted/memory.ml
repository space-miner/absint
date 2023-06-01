open Base
open Interval

type variable = string

module Memory : sig
  type t

  val join : t -> t -> t
end = struct
  type t = (variable, Interval.t) Hashtbl.t

  let join (t1 : t) (t2 : t) =
    Hashtbl.fold
      ~init:t1
      ~f:(fun ~key:variable ~data:interval acc ->
        match Hashtbl.find acc variable with
        | None -> acc
        | Some interval' ->
          let interval'' = Interval.join interval interval' in
          let _ = Hashtbl.set acc ~key:variable ~data:interval'' in
          acc)
      t2
  ;;
end
