open Base
open Syntax

  let rec absint_expression expression memory =
    match expression with
    | Const c -> Interval.Interval (Bigint.Int c, Bigint.Int c)
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

  let absint_neg_condition
    (Cmp (compare_operation, left_expression, right_expression))
    (memory : Memory.t)
    : Interval.t
    =
    match left_expression with
    | Var _ ->
      (match right_expression with 
      | Var _ -> failwith "Wrong Cond Form" 
      | _ ->
      let right_interval = absint_expression right_expression memory in
      (match compare_operation with
       | Less ->
         let modified_right_interval =
           match right_interval with
           | Interval.Interval (_, hi) -> Interval.Interval (hi, Bigint.PosInf)
           | Interval.Bottom -> Interval.Bottom
         in
         modified_right_interval
       | Equal -> Interval.Interval (Bigint.NegInf, Bigint.PosInf)))
    | _ -> failwith "Wrong Cond Form."
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
         let modified_right_interval =
           match right_interval with
           | Interval.Interval (_, hi) ->
             (Interval.( - )
               (Interval.Interval (Bigint.NegInf, hi))
               (Interval.Interval (Bigint.Int Z.one, Bigint.Int Z.one)))
           | Interval.Bottom -> Interval.Bottom
         in
         modified_right_interval
       | Equal ->
         let left_interval = Option.value (Hashtbl.find memory x) ~default:Bottom in
         let joined_interval = Interval.join left_interval right_interval in
         joined_interval)
    | _ -> failwith "Wrong Cond Form."
  ;;

  let rec absint_command currCmd glblState nextLabel =
    match currCmd with
    | Seq (lbl, cmd1, cmd2) ->
      let first_label = Util.find_label cmd1 in
      let nextMem = Util.get_global_mem glblState lbl in
      let _ = Hashtbl.set glblState ~key:first_label ~data:nextMem in
      absint_command cmd1 glblState (Util.find_label cmd2)
    | While (while_label, cond, cmd) ->
      let curMem = Util.get_global_mem glblState while_label in
      let cond_interval = absint_condition cond curMem in
      let neg_cond_interval = absint_neg_condition cond curMem in
      let cmd_label = Util.find_label cmd in
      let var =
        match cond with
        | Cmp (_, Var x, _) -> x
        | _ ->
          failwith
            "No variable in Condition expression. public static void assume \
             absint_command failure."
      in
      let var_interval = Option.value (Hashtbl.find curMem var) ~default:Bottom in
      let meet_interval1 = Interval.meet cond_interval var_interval in
      let meet_interval2 = Interval.meet neg_cond_interval var_interval in
      let meets =
        match meet_interval1, meet_interval2 with
        | Bottom, Bottom -> []
        | Interval.Interval m1, Bottom -> [ cmd_label, Interval.Interval m1 ]
        | Bottom, Interval.Interval m2 -> [ nextLabel, Interval.Interval m2 ]
        | Interval.Interval m1, Interval.Interval m2 -> [ cmd_label, Interval.Interval m1; nextLabel, Interval.Interval m2 ]
      in
      List.fold meets ~init:[] ~f:(fun acc (lbl, meet) ->
        let curMemPrime = Hashtbl.copy curMem in
        let _ = Hashtbl.set curMemPrime ~key:var ~data:meet in
        let nextMem = Util.get_global_mem glblState lbl in
        if Memory.( <> ) curMemPrime nextMem
        then (
          let joinMem = Memory.join curMemPrime nextMem in
          let _ = Hashtbl.set glblState ~key:lbl ~data:joinMem in
          lbl :: acc)
        else if Hashtbl.is_empty curMemPrime && Hashtbl.is_empty nextMem
        then lbl :: acc
        else acc)
    | Choice (lbl, cmd1, cmd2) ->
      let curMem = Util.get_global_mem glblState lbl in
      let label1 = Util.find_label cmd1 in
      let label2 = Util.find_label cmd2 in
      List.fold [ label1; label2 ] ~init:[] ~f:(fun acc label ->
        let nextMem = Util.get_global_mem glblState label in
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
      let curMem = Util.get_global_mem glblState lbl in
      let cond_eval = absint_condition cond curMem in
      let _ = Hashtbl.set curMem ~key:v ~data:cond_eval in
      let nextMem = Util.get_global_mem glblState nextLabel in
      if Memory.( <> ) curMem nextMem
      then (
        let joinMem = Memory.join curMem nextMem in
        Hashtbl.set glblState ~key:nextLabel ~data:joinMem;
        [ nextLabel ])
      else []
    | Assign (lbl, var, exp) ->
      let curMem = Util.get_global_mem glblState lbl in
      let oldBint = Option.value (Hashtbl.find curMem var) ~default:Bottom in
      let newBint = absint_expression exp curMem in
      let joinBint = Interval.join oldBint newBint in
      let _ = Hashtbl.set curMem ~key:var ~data:joinBint in
      let nextMem = Util.get_global_mem glblState nextLabel in
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

