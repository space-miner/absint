open Interval
open Syntax
open Base

let rec absintExpr e mem =
  match e with
  | Const c -> Some (BigInt.Int c, BigInt.Int c)
  | Var x ->
    (match Map.find mem x with
     | Some intervalOpt -> intervalOpt
     | _ -> None)
  | Binop (binop, e1, e2) -> intervalBinop binop (absintExpr e1 mem) (absintExpr e2 mem)
;;

let firstLabel cmd =
  match cmd with
  | Seq (l, _, _) -> Some l
  | Assume (l, _) -> Some l
  | While (l, _, _) -> Some l
  | Choice (l, _, _) -> Some l
  | Assign (l, _, _) -> Some l
;;

let rec findCmd prog label =
  match prog with 
  | Seq (l, c1, c2) -> (
      if label = l then Some (Seq (l, c1, c2)) 
      else 
        match findCmd c1 label with
        | Some c -> Some c
        | None ->
          match findCmd c2 label with
          | Some c -> Some c 
          | None -> None)
  | Assume (l, c) -> 
      if label = l then Some (Assume (l, c)) 
      else None
  | While (l,cond,c) -> (
      if label = l then Some (While (l,cond,c)) else
      match findCmd c label with
      | Some c -> Some c
      | None -> None)
  | Choice (l, c1, c2) -> (
    if label = l then Some (Choice (l, c1, c2)) 
    else 
      match findCmd c1 label with
      | Some c -> Some c
      | None ->
        match findCmd c2 label with
        | Some c -> Some c 
        | None -> None)
  | Assign (l,v,e) ->
      if label = l then Some (Assign (l,v,e)) else None


let absintCond _cmpop _e1 _e2 = failwith "IDK"
let absintCmd = failwith "todo"
let absintStep _ _ _ = failwith "todo"

let rec absintIterHelper stack global (Prog (c, l)) = 
if Stack.is_empty stack
then 
  () 
else 
  let label, mem = Stack.pop_exn stack in
  let nextCmd = findCmd c label in
  match nextCmd with 
  | Some c -> 
      let _ = absintStep label c mem in 
      absintIterHelper stack global (Prog (c, l))
  | None -> failwith "label not found in AST"
  

let absintIter (Prog (cmd, l)) initMem =
  let initLabel' = firstLabel cmd in
  let initLabel = 
    match initLabel' with 
      | Some l -> l 
      | _ -> failwith "IDK"
  in 
  let worklist = Stack.of_list [ initLabel, initMem ] in
  let glbl = IntMap.empty in
  let glbl' = IntMap.add initLabel initMem glbl in
  absintIterHelper worklist glbl' (Prog (cmd, l))
;;
