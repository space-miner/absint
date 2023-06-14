open Base
open Interval
open Bigint
open Syntax
open Util

module Memory : sig
  type t = (var, Interval.t) Hashtbl.t

  val ( == ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val join : t -> t -> t
end = struct
  type t = (var, Interval.t) Hashtbl.t

  let is_subset t1 t2 =
    Hashtbl.fold
      ~init:true
      ~f:(fun ~key:var ~data:interval1 bool_acc ->
        let interval2 = Option.value (Hashtbl.find t2 var) ~default:None in
        Interval.is_subset interval1 interval2 && bool_acc)
      t1
  ;;

  let ( == ) t1 t2 = is_subset t1 t2 && is_subset t2 t1
  let ( <> ) t1 t2 = not (t1 == t2)

  let join t1 t2 =
    Hashtbl.fold
      ~init:t1
      ~f:(fun ~key:variable ~data:interval acc ->
        match Hashtbl.find t1 variable with
        | None -> acc
        | Some interval' ->
          let interval'' = Interval.join interval interval' in
          let _ = Hashtbl.set acc ~key:variable ~data:interval'' in
          acc)
      t2
  ;;
end

module Absint : sig
  val absint_expression : expr -> Memory.t -> Interval.t
  val absint_command : cmd -> (label, Memory.t) Hashtbl.t -> label -> label list
  val absint_iter : prog -> Memory.t -> (label, (var, Interval.t) Hashtbl.t) Hashtbl.t

  val absint_iter_loop
    :  label Base.Stack.t
    -> (label, Memory.t) Hashtbl.t
    -> cmd
    -> label
    -> (label, Memory.t) Hashtbl.t
end = struct
  let rec absint_expression expression memory =
    match expression with
    | Const c -> Some (BigInt.Int c, BigInt.Int c)
    | Var x ->
      (match Hashtbl.find memory x with
       | Some x -> x
       | None -> failwith "todo, none or [-inf, inf]")
    | Binop (operation, left_expression, right_expression) ->
      (match operation with
       | Add ->
         Interval.( + )
           (absint_expression left_expression memory)
           (absint_expression right_expression memory)
       | Sub ->
         Interval.( - )
           (absint_expression right_expression memory)
           (absint_expression right_expression memory))
  ;;

  let absint_condition
    (Cmp (compare_operation, left_expression, right_expression))
    (memory : Memory.t)
    : Interval.t
    =
    match left_expression with
    | Var x ->
      let right_interval = absint_expression right_expression memory in
      (match compare_operation with
       | Less ->
         (* if right interval is [4,11] modified will be [-inf, 10] -- to generalize [a,b] -> [-inf, b-1]*)
         let modified_right_interval =
           match right_interval with
           | Some (_, hi) ->
             Interval.( - )
               (Some (BigInt.NegInf, hi))
               (Some (BigInt.Int Z.one, BigInt.Int Z.one))
           | None -> None
         in
         let left_interval = Option.value (Hashtbl.find memory x) ~default:None in
         let joined_interval = Interval.join left_interval modified_right_interval in
         joined_interval
       | Equal ->
         (* 
          we can overwrite x with the right_interval if the worklist processes items inorder -- i.e. 
          it never processes something that comes before an assume after processing an assume.contents

          right now we're just super over approximating:
        *)
         let left_interval = Option.value (Hashtbl.find memory x) ~default:None in
         let joined_interval = Interval.join left_interval right_interval in
         joined_interval)
    | _ -> failwith "we only ac(opium)cept variables on left and expressions on right"
  ;;

  let rec absint_command currCmd glblState nextLabel =
    match currCmd with
    | Seq (_, cmd1, cmd2) -> absint_command cmd1 glblState (Util.find_label cmd2)
    | While (lbl, cond, cmd) ->
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let cond_interval = absint_condition cond curMem in
      let label = Util.find_label cmd in
      let var =
        match cond with
        | Cmp (_, Var x, _) -> x
        | _ ->
          failwith
            "No variable in Condition expression. public static void assume \
             absint_command failure."
      in
      let _ = Stdio.printf "%d\n" lbl in
      let var_interval = Option.value (Hashtbl.find curMem var) ~default:None in
      let meet_interval1 = Interval.meet cond_interval var_interval in
      let meet_interval2 = Interval.meet (Interval.not cond_interval) var_interval in
      List.fold
        [ label, meet_interval1; nextLabel, meet_interval2 ]
        ~init:[]
        ~f:(fun acc (lbl, meet) ->
          let _ = Stdio.printf "inside fold %d\n" lbl in
          let curMemPrime = Hashtbl.copy curMem in
          let _ = Hashtbl.set curMemPrime ~key:var ~data:meet in
          let nextMem =
            Option.value
              (Hashtbl.find glblState lbl)
              ~default:(Hashtbl.create (module String))
          in
          if Memory.( <> ) curMemPrime nextMem
          then (
            let joinMem = Memory.join curMemPrime nextMem in
            let _ = Hashtbl.iteri joinMem ~f:(fun ~key:var ~data:data -> 
                          Stdio.printf "joinMem label:%d var:%s interval:%s\n" lbl (var) (Interval.to_string data))
            in
            let _ = Hashtbl.set glblState ~key:lbl ~data:joinMem in
            lbl :: acc)
          else if Hashtbl.is_empty curMemPrime && Hashtbl.is_empty nextMem
          then lbl :: acc
          else acc) 
    | Choice (lbl, cmd1, cmd2) ->
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let label1 = Util.find_label cmd1 in
      let label2 = Util.find_label cmd2 in
      List.fold [ label1; label2 ] ~init:[] ~f:(fun acc label ->
        let nextMem =
          Option.value
            (Hashtbl.find glblState label)
            ~default:(Hashtbl.create (module String))
        in
        if Memory.( <> ) curMem nextMem
        then (
          let joinMem = Memory.join curMem nextMem in
          let _ = Hashtbl.set glblState ~key:label ~data:joinMem in
          label :: acc)
        else if Hashtbl.is_empty curMem && Hashtbl.is_empty nextMem
        then label :: acc
        else acc)
    | Assume (lbl, cond) ->
      let v =
        match cond with
        | Cmp (_, Var x, _) -> x
        | _ ->
          failwith
            "No variable in Condition expression. public static void assume \
             absint_command failure."
      in
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let cond_eval = absint_condition cond curMem in
      let _ = Hashtbl.set curMem ~key:v ~data:cond_eval in
      let nextMem =
        Option.value
          (Hashtbl.find glblState nextLabel)
          ~default:(Hashtbl.create (module String))
      in
      if Memory.( <> ) curMem nextMem
      then (
        let joinMem = Memory.join curMem nextMem in
        Hashtbl.set glblState ~key:nextLabel ~data:joinMem;
        [ nextLabel ])
      else []
    | Assign (lbl, var, exp) ->
      let curMem =
        match Hashtbl.find glblState lbl with
        | None -> Hashtbl.create (module String)
        | Some mem -> Hashtbl.copy mem
      in
      let oldBint = Option.value (Hashtbl.find curMem var) ~default:None in
      let newBint = absint_expression exp curMem in
      let joinBint = Interval.join oldBint newBint in
      let _ = Hashtbl.set curMem ~key:var ~data:joinBint in
      let nextMem =
        Option.value
          (Hashtbl.find glblState nextLabel)
          ~default:(Hashtbl.create (module String))
      in
      if Memory.( <> ) curMem nextMem
      then (
        let joinMem = Memory.join curMem nextMem in
        Hashtbl.set glblState ~key:nextLabel ~data:joinMem;
        [ nextLabel ])
      else []
  ;;

  let rec absint_iter_loop
    (stack : label Stack.t)
    (global : (label, Memory.t) Hashtbl.t)
    (constProg : cmd)
    (lExit : label)
    =
    if Stack.is_empty stack
    then global
    else (
      let label = Stack.pop_exn stack in
      match Util.find_command constProg label with
      | None -> absint_iter_loop stack global constProg lExit
      | Some command ->
        (match Util.find_next_label constProg label lExit with
         | None -> failwith "find_next_label err in absint_iterLoop"
         | Some nextLabel ->
           let labels = absint_command command global nextLabel in
           let _ = List.map labels ~f:(fun x -> Stack.push stack x) in
           absint_iter_loop stack global constProg lExit))
  ;;

  let absint_iter (Prog (command, label)) initial_memory =
    let init_label = Util.find_label command in
    let worklist = Base.Stack.of_list [ init_label ] in
    let global = Hashtbl.create (module Int) in
    let _ = Hashtbl.set global ~key:init_label ~data:initial_memory in
    absint_iter_loop worklist global command label
  ;;
end
