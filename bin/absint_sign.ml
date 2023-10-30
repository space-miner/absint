open Base
open Syntax

let rec absint_expression expression memory =
  match expression with
  | Const c -> Sign_domain.of_const c
  | Var x ->
    (match Hashtbl.find memory x with
     | Some x -> x
     | None -> failwith "todo, bottom or top")
  | Binop (op, left, right) ->
    (match op with
     | Add ->
       Sign_domain.( + ) (absint_expression left memory) (absint_expression right memory)
     | Sub ->
       Sign_domain.( - ) (absint_expression left memory) (absint_expression right memory))
;;

let absint_neg_condition_sign
  (Cmp (compare_operation, left_expression, right_expression))
  (memory : Memory_sign.t)
  : Sign_domain.t
  =
  match left_expression with
  | Var _ ->
    let right_sign = absint_expression right_expression memory in
    (match compare_operation with
     | Less ->
       let neg_right_sign =
         match right_sign with
         | Zero -> Sign_domain.Positive
         | Negative -> Sign_domain.NonNegative
         | NonPositive -> Sign_domain.Positive
         | _ -> Sign_domain.Bottom
       in
       neg_right_sign
     | Equal -> Bottom)
  | _ -> failwith "Invalid Cond Form."
;;

let absint_cond_left left_expression (memory : Memory_sign.t) =
  match left_expression with
  | Var x -> Option.value (Hashtbl.find memory x) ~default:Bottom
  | _ -> failwith "Left Expression of Cond is not a Variable"
;;

let absint_condition_sign
  (Cmp (compare_operation, left_expression, right_expression))
  (memory : Memory_sign.t)
  : Sign_domain.t
  =
  match compare_operation with
  | Less ->
    let left = absint_cond_left left_expression memory in
    let right = absint_expression right_expression memory in
    (match left, right with
     | Bottom, _ | _, Bottom -> Bottom
     | _, Positive -> NonPositive
     | _, Zero -> Negative
     | _, NonNegative -> Negative
     | _, _ -> Bottom)
  | Equal ->
    let left = absint_cond_left left_expression memory in
    let right = absint_expression right_expression memory in
    (match left, right with
     | Bottom, _ | _, Bottom -> Bottom
     | _ -> right)
;;

let rec absint_command current_command global next_label =
  match current_command with
  | Seq (lbl, cmd1, cmd2) ->
    let cmd1_label = Util.find_label cmd1 in
    let curr_mem = Util.get_global_mem global lbl in
    let _ = Hashtbl.set global ~key:cmd1_label ~data:curr_mem in
    absint_command cmd1 global (Util.find_label cmd2)
  | While (while_label, cond, cmd) ->
    let curr_mem = Util.get_global_mem global while_label in
    let cond_sign = absint_condition_sign cond curr_mem in
    let neg_cond_sign = absint_neg_condition_sign cond curr_mem in
    let cmd_label = Util.find_label cmd in
    let var =
      match cond with
      | Cmp (_, Var x, _) -> x
      | _ -> failwith "No variable in Condition expression."
    in
    let var_sign = Option.value (Hashtbl.find curr_mem var) ~default:Bottom in
    let meet_sign1 = Sign_domain.meet cond_sign var_sign in
    let meet_sign2 = Sign_domain.meet neg_cond_sign var_sign in
    let meets =
      match meet_sign1, meet_sign2 with
      | Bottom, Bottom -> []
      | sign, Bottom -> [ cmd_label, sign ]
      | Bottom, sign -> [ next_label, sign ]
      | sign1, sign2 -> [ cmd_label, sign1; next_label, sign2 ]
    in
    List.fold meets ~init:[] ~f:(fun acc (lbl, meet) ->
      let curr_mem_prime = Hashtbl.copy curr_mem in
      let _ = Hashtbl.set curr_mem_prime ~key:var ~data:meet in
      let next_mem = Util.get_global_mem global lbl in
      if Memory_sign.( <> ) curr_mem_prime next_mem
      then (
        let join_mem = Memory_sign.join curr_mem_prime next_mem in
        let _ = Hashtbl.set global ~key:lbl ~data:join_mem in
        lbl :: acc)
      else if Hashtbl.is_empty curr_mem_prime && Hashtbl.is_empty next_mem
      then lbl :: acc
      else acc)
  | Choice (lbl, cmd1, cmd2) ->
    let curr_mem = Util.get_global_mem global lbl in
    let cmd1_label = Util.find_label cmd1 in
    let cmd2_label = Util.find_label cmd2 in
    List.fold [ cmd1_label; cmd2_label ] ~init:[] ~f:(fun acc label ->
      let next_mem = Util.get_global_mem global label in
      if Memory_sign.( <> ) curr_mem next_mem
      then (
        let join_mem = Memory_sign.join curr_mem next_mem in
        let _ = Hashtbl.set global ~key:label ~data:join_mem in
        label :: acc)
      else if Hashtbl.is_empty curr_mem && Hashtbl.is_empty next_mem
      then label :: acc
      else acc)
  | Assume (lbl, cond) ->
    let var =
      match cond with
      | Cmp (_, Var x, _) -> x
      | _ -> failwith "No variable in Condition expression."
    in
    let curr_mem = Util.get_global_mem global lbl in
    let cond_sign = absint_condition_sign cond curr_mem in
    let _ = Hashtbl.set curr_mem ~key:var ~data:cond_sign in
    let next_mem = Util.get_global_mem global next_label in
    if Memory_sign.( <> ) curr_mem next_mem
    then (
      let join_mem = Memory_sign.join curr_mem next_mem in
      Hashtbl.set global ~key:next_label ~data:join_mem;
      [ next_label ])
    else []
  | Assign (lbl, var, exp) ->
    let curr_mem = Util.get_global_mem global lbl in
    let var_sign = Option.value (Hashtbl.find curr_mem var) ~default:Bottom in
    let exp_sign = absint_expression exp curr_mem in
    let join_sign = Sign_domain.join var_sign exp_sign in
    let _ = Hashtbl.set curr_mem ~key:var ~data:join_sign in
    let next_mem = Util.get_global_mem global next_label in
    if Memory_sign.( <> ) curr_mem next_mem
    then (
      let join_mem = Memory_sign.join curr_mem next_mem in
      Hashtbl.set global ~key:next_label ~data:join_mem;
      [ next_label ])
    else []
;;

let rec absint_iter_loop
  (stack : label Stack.t)
  (global : (label, Memory_sign.t) Hashtbl.t)
  (prog : cmd)
  (exit_label : label)
  =
  if Stack.is_empty stack
  then global
  else (
    let label = Stack.pop_exn stack in
    match Util.find_command prog label with
    | None -> absint_iter_loop stack global prog exit_label
    | Some command ->
      (match Util.find_next_label prog label exit_label with
       | None -> failwith "find_next_label err in absint_iter_loop"
       | Some next_label ->
         let labels = absint_command command global next_label in
         let _ = List.map labels ~f:(fun x -> Stack.push stack x) in
         absint_iter_loop stack global prog exit_label))
;;

let absint_iter (Prog (command, exit_label)) initial_memory =
  let init_label = Util.find_label command in
  let worklist = Base.Stack.of_list [ init_label ] in
  let global = Hashtbl.create (module Int) in
  let _ = Hashtbl.set global ~key:init_label ~data:initial_memory in
  absint_iter_loop worklist global command exit_label
;;
