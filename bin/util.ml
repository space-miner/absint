open Syntax

module Util : sig
  val find_label : cmd -> int
  val find_next_label : cmd -> int -> int -> int option
  val find_command : cmd -> int -> cmd option
end = struct
  let find_label command =
    match command with
    | Seq (l, _, _) | Assume (l, _) | While (l, _, _) | Choice (l, _, _) | Assign (l, _, _)
      -> l
  ;;

  let rec find_next_label command label next_label =
    match command with
    | Seq (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some (find_label cmd1)
      else (
        match find_next_label cmd1 label (find_label cmd2) with
        | None -> find_next_label cmd2 label next_label
        | label_opt -> label_opt)
    | Assume (lbl, _) -> if lbl = label then Some next_label else None
    | While (lbl, _, cmd) ->
      if lbl = label
      then Some next_label
      else (
        match find_next_label cmd label lbl with
        | None -> None
        | label_opt -> label_opt)
    | Choice (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some next_label
      else (
        match find_next_label cmd1 label next_label with
        | Some lbl -> Some lbl
        | None -> find_next_label cmd2 label next_label)
    | Assign (lbl, _, _) -> if lbl = label then Some next_label else None
  ;;

  let rec find_command command label =
    match command with
    | Seq (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some (Seq (lbl, cmd1, cmd2))
      else (
        match find_command cmd1 label with
        | Some cmd -> Some cmd
        | None ->
          (match find_command cmd2 label with
           | Some cmd -> Some cmd
           | None -> None))
    | Assume (lbl, _cmd) -> if lbl = label then Some command else None
    | While (lbl, _cond, cmd) ->
      if lbl = label
      then Some command
      else (
        match find_command cmd label with
        | Some cmd -> Some cmd
        | None -> None)
    | Choice (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some command
      else (
        match find_command cmd1 label with
        | Some cmd -> Some cmd
        | None ->
          (match find_command cmd2 label with
           | Some cmd -> Some cmd
           | None -> None))
    | Assign (lbl, _var, _expr) -> if lbl = label then Some command else None
  ;;
end
