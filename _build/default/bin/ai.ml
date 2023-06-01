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

let rec absintIterLoop stack global constProg lExit =  
if Stack.is_empty stack
then 
  global
else 
  let label, mem = Stack.pop_exn stack in
  match findCmd constProg label with 
    | None -> failwith "findCmd err in absintIterLoop"
    | Some nextCommand -> 
        match findNextLabel constProg label lExit with 
        | None -> failwith "findNextLabel err in absintIterLoop"
        | Some nextLabel -> 
          let memOpt = 
            if IntMap.mem nextLabel global 
            then 
              IntMap.find label global
            else
              memOpt = None 
          in 
            let _ = absintStep nextCommand memOpt 
            in global


let absintIter (Prog (cmd, l)) initMem =
  let initLabel' = firstLabel cmd in
  let initLabel = 
    match initLabel' with 
      | Some l -> l 
      | _ -> failwith "This really shouldn't happen"
  in 
  let worklist = Stack.of_list [ initLabel, initMem ] in
  let glbl = IntMap.empty in
  let glbl' = IntMap.add initLabel initMem glbl in
  absintIterLoop worklist glbl' cmd l
;;
