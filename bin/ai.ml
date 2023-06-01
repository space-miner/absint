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
  | Seq (l, _, _, _) -> l
  | Assume (l, _) -> l
  | While (l, _, _) -> l
  | Choice (l, _, _) -> l
  | Assign (l, _, _) -> l
;;

let absintCond _cmpop _e1 _e2 = failwith "IDK"
let absintCmd = failwith "todo"
let absintStep = failwith "todo"

let rec absintIterHelper stack global = 
if Stack.is_empty stack
then 
  () 
else (
  let label, mem = Stack.pop_exn stack in
  let  = doSomethingWith label mem in
  absintIterHelper stack global)

let absintIter (Prog cmd) initMem =
  let initLabel = firstLabel cmd in
  let worklist = Stack.of_list [ initLabel, initMem ] in
  let glbl = IntMap.empty in
  let glbl' = IntMap.add initLabel initMem glbl in
  absintIterHelper worklist glbl' 
;;
