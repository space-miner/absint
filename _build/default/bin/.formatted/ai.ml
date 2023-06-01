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

let rec absintStep label command nextLabel mem = []

let rec absintIterLoop stack global constProg lExit =
  if Stack.is_empty stack
  then global
  else (
    let label = Stack.pop_exn stack in
    match findCmd constProg label with
    | None -> failwith "findCmd err in absintIterLoop"
    | Some command ->
      (match findNextLabel constProg label lExit with
       | None -> failwith "findNextLabel err in absintIterLoop"
       | Some nextLabel ->
         let mem =
           if IntMap.mem label global then IntMap.find label global else StringMap.empty
         in
         let mems = absintStep label command nextLabel mem in
         global))
;;

let absintIter (Prog (cmd, l)) initMem =
  let initLabel' = firstLabel cmd in
  let initLabel =
    match initLabel' with
    | Some l -> l
    | _ -> failwith "This really shouldn't happen"
  in
  let worklist = Stack.of_list [ initLabel ] in
  let glbl = IntMap.empty in
  let glbl' = IntMap.add initLabel initMem glbl in
  absintIterLoop worklist glbl' cmd l
;;
