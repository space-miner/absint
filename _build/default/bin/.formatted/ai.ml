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

let absintIter (Prog cmd) initMem =
  let initLabel = firstLabel cmd in
  let rec absintIterHelper stk =
    if Stack.is_empty stk
    then ()
    else (
      let label, mem = Stack.pop_exn stk in
      let _ = doSomethingWith label mem in
      let stk' = maybeAddToStack stk in
      absintIterHelper stk')
  in
  let worklist = Stack.of_list [ initLabel, initMem ] in
  absintIterHelper worklist
;;
