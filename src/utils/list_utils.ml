let sum = List.fold_left ( + ) 0

let int_list str_list =
  List.map int_of_string str_list

let first_element lst =
  match lst with
  | [] -> failwith "Empty list has no first element"
  | x :: _ -> x
