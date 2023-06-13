open Base
open Binterval
open Extinct
open Syntax
open Util

module Memory : sig
  type t = (var, Binterval.t) Hashtbl.t

  val ( == ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val join : t -> t -> t
end = struct
  type t = (var, Binterval.t) Hashtbl.t

  let is_subset t1 t2 =
    Hashtbl.fold
      ~init:true
      ~f:(fun ~key:var ~data:bint1 bool_acc ->
        let bint2 =
          match Hashtbl.find t2 var with
          | None -> None
          | Some x -> x
        in
        Binterval.is_subset bint1 bint2 && bool_acc)
      t1
  ;;

  let ( == ) (t1 : t) (t2 : t) = is_subset t1 t2 && is_subset t2 t1
  let ( <> ) (t1 : t) (t2 : t) = not (t1 == t2)

  let join (t1 : t) (t2 : t) =
    Hashtbl.fold
      ~init:t1
      ~f:(fun ~key:variable ~data:interval acc ->
        match Hashtbl.find t1 variable with
        | None -> acc
        | Some interval' ->
          let interval'' = Binterval.join interval interval' in
          let _ = Hashtbl.set acc ~key:variable ~data:interval'' in
          acc)
      t2
  ;;
end

module Absint : sig
  val absintExpr : expr -> Memory.t -> Binterval.t
  val absintCmd : cmd -> (label, Memory.t) Hashtbl.t -> label -> label list
  val absintIter : prog -> Memory.t -> (label, (var, Binterval.t) Hashtbl.t) Hashtbl.t

  val absintIterLoop
    :  label Base.Stack.t
    -> (label, Memory.t) Hashtbl.t
    -> cmd
    -> label
    -> (label, Memory.t) Hashtbl.t
end = struct
  let rec absintExpr exp (mem : Memory.t) =
    match exp with
    | Const c -> Some (Extinct.Int c, Extinct.Int c)
    | Var x ->
      (match Hashtbl.find mem x with
       | Some x -> x
       | None -> failwith "todo, none or [-inf, inf]")
    | Binop (op, exp1, exp2) ->
      (match op with
       | Add -> Binterval.( + ) (absintExpr exp1 mem) (absintExpr exp2 mem)
       | Sub -> Binterval.( - ) (absintExpr exp1 mem) (absintExpr exp2 mem))
  ;;

  let rec absintCmd currCmd glblState nextLabel =
    match currCmd with
    | Seq (_, cmd1, cmd2) -> absintCmd cmd1 glblState (Util.findLabel cmd2)
    | Choice (lbl, cmd1, cmd2) ->
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let label1 = Util.findLabel cmd1 in
      let label2 = Util.findLabel cmd2 in
      List.fold [ label1; label2 ] ~init:[] ~f:(fun acc label ->
        let nextMem =
          match Hashtbl.find glblState label with
          | None -> Hashtbl.create (module String)
          | Some mem -> mem
        in
        if Memory.( <> ) curMem nextMem
        then 
          let joinMem = Memory.join curMem nextMem in
          let _ = Hashtbl.set glblState ~key:label ~data:joinMem in
          label :: acc
        else if Hashtbl.is_empty curMem && Hashtbl.is_empty nextMem
        then label :: acc
        else acc)
    | Assign (lbl, var, exp) ->
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let oldBint =
        match Hashtbl.find curMem var with
        | None -> None
        | Some b -> b
      in
      let newBint = absintExpr exp curMem in
      let joinBint = Binterval.join oldBint newBint in
      let _ = Hashtbl.set curMem ~key:var ~data:joinBint in
      let nextMem =
        match Hashtbl.find glblState nextLabel with
        | None -> Hashtbl.create (module String)
        | Some mem -> mem
      in
      if Memory.( <> ) curMem nextMem
      then (
        let joinMem = Memory.join curMem nextMem in
        let _ = Hashtbl.set glblState ~key:nextLabel ~data:joinMem in
        [ nextLabel ])
      else []
    | _ -> failwith "todo"
  ;;

  let rec absintIterLoop
    (stack : label Stack.t)
    (global : (label, Memory.t) Hashtbl.t)
    (constProg : cmd)
    (lExit : label)
    =
    if Stack.is_empty stack
    then global
    else (
      let label = Stack.pop_exn stack in
      match Util.findCmd constProg label with
      | None -> absintIterLoop stack global constProg lExit
      | Some command ->
        (match Util.findNextLabel constProg label lExit with
         | None -> failwith "findNextLabel err in absintIterLoop"
         | Some nextLabel ->
           let labels = absintCmd command global nextLabel in
           let _ = List.map labels ~f:(fun x -> Stack.push stack x) in
           absintIterLoop stack global constProg lExit))
  ;;

  let absintIter (Prog (cmd, l)) initMem =
    let initLabel = Util.findLabel cmd in
    let worklist = Base.Stack.of_list [ initLabel ] in
    let glbl = Hashtbl.create (module Int) in
    let _ = Hashtbl.set glbl ~key:initLabel ~data:initMem in
    absintIterLoop worklist glbl cmd l
  ;;
end
