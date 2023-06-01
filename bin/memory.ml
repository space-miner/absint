open Base
open Interval

type variable = string

module Memory = struct
  type t = (variable, Interval.t) Hashtbl.t

  let join _t _t' = failwith "todo"
end
