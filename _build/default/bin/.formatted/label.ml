open Syntax

let firstLabel cmd =
  match cmd with
  | Seq (l, _, _) | Assume (l, _) | While (l, _, _) | Choice (l, _, _) | Assign (l, _, _)
    -> l
;;

let rec findNextLabel command label (nextLabel : int) : int option =
  match command with
  | Seq (l, c1, c2) ->
    if l = label
    then Some (firstLabel c1)
    else (
      match findNextLabel c1 label (firstLabel c2) with
      | None -> findNextLabel c2 label nextLabel
      | labelOpt -> labelOpt)
  | Assume (l, _) -> if l = label then Some nextLabel else None
  | While (l, _, c) ->
    if l = label
    then Some nextLabel
    else (
      match findNextLabel c label nextLabel with
      | None -> None
      | labelOpt -> labelOpt)
  | Choice (l, c1, c2) ->
    if l = label
    then Some nextLabel
    else (
      match findNextLabel c1 label nextLabel with
      | Some l -> Some l
      | None -> findNextLabel c2 label nextLabel)
  | Assign (l, _, _) -> if l = label then Some nextLabel else None
;;

let rec findCmd command label =
  match command with
  | Seq (l, c1, c2) ->
    if label = l
    then Some (Seq (l, c1, c2))
    else (
      match findCmd c1 label with
      | Some c -> Some c
      | None ->
        (match findCmd c2 label with
         | Some c -> Some c
         | None -> None))
  | Assume (l, c) -> if label = l then Some (Assume (l, c)) else None
  | While (l, cond, c) ->
    if label = l
    then Some (While (l, cond, c))
    else (
      match findCmd c label with
      | Some c -> Some c
      | None -> None)
  | Choice (l, c1, c2) ->
    if label = l
    then Some (Choice (l, c1, c2))
    else (
      match findCmd c1 label with
      | Some c -> Some c
      | None ->
        (match findCmd c2 label with
         | Some c -> Some c
         | None -> None))
  | Assign (l, v, e) -> if label = l then Some (Assign (l, v, e)) else None
;;
