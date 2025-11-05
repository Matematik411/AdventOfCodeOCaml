open Solvers.Signature
open Utils.List_utils

let sum_all_numbers string =
  let rec parse int_list current_num char_seq =
    match char_seq () with 
    | Seq.Nil -> (int_of_string current_num) :: int_list
    | Seq.Cons (c, rest) -> 
        match int_of_string_opt (String.make 1 c) with
        | Some d -> parse int_list (current_num ^ (string_of_int d)) rest
        | None when c = '-' -> parse int_list "-" rest (* minus is always followed by a number*)
        | None when current_num = "0" -> parse int_list "0" rest
        | None -> parse ((int_of_string current_num) :: int_list) "0" rest
  in
  parse [] "0" (String.to_seq string)
  |> List.fold_left (+) 0

let rec sum_numbers_skip_red json =
  match json with
  | `Int n -> n
  | `Float f -> int_of_float f
  | `List items -> 
      List.fold_left (fun acc item -> acc + sum_numbers_skip_red item) 0 items
  | `Assoc fields ->
      (* Check if any value is the string "red" *)
      let has_red = List.exists (fun (_, v) -> v = `String "red") fields in
      if has_red then 0
      else List.fold_left (fun acc (_, v) -> acc + sum_numbers_skip_red v) 0 fields
  | _ -> 0

module Solver : Solver = struct
  
  let part1 lines = 
    sum_all_numbers (first_element lines) |> string_of_int
  
  let part2 lines = 
    let json_string = first_element lines in
    let json = Yojson.Basic.from_string json_string in
    sum_numbers_skip_red json |> string_of_int

end