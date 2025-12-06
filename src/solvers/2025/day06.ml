open Solvers.Signature



(* example input:
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +   *)

(* parse input into columns (int array) array where i put -1 for + and -2 for * *)
(* count len of lines and initialize array of zeroes for each column *)
let parse_lines_one lines =
  let column_length = List.length lines in
  let num_columns =
    match lines with
    | [] -> 0
    | first_line :: _ ->
        List.length (String.split_on_char ' ' first_line |> List.filter (fun s -> s <> ""))
  in
  let columns = Array.init num_columns (fun _ -> Array.init column_length (fun _ -> 0)) in

  List.iteri (fun row_idx line ->
    let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
    List.iteri (fun col_idx part ->
      let value = 
        match part with
        | "+" -> -1
        | "*" -> -2
        | _ -> int_of_string part
      in
      columns.(col_idx).(row_idx) <- value
    ) parts
  ) lines;
  columns

let solve_one columns = 
  Array.fold_left ( fun acc col ->
    if Array.exists ((=) (-1)) col then 
      acc + Array.fold_left (fun a v -> if v > 0 then a + v else a) 0 col
    else
      acc + Array.fold_left (fun a v -> if v > 0 then a * v else a) 1 col
  ) 0 columns


let parse_lines_two lines =
  let lines_array = Array.of_list lines in
  let num_rows = Array.length lines_array in
  if num_rows = 0 then [||]
  else
    let last_row = lines_array.(num_rows - 1) in
    let data_rows = Array.sub lines_array 0 (num_rows - 1) in
    let max_width = String.length last_row in
    
    (* don't know how to do it better than looping over the line *)
    let rec process_position i numbers operation acc =
      if i >= max_width then
        List.rev ((numbers, operation) :: acc) |> Array.of_list
      else

        let number_str = 
          Array.fold_left (fun str row ->
            if i < String.length row && row.[i] <> ' ' then
              str ^ String.make 1 row.[i]
            else
              str
          ) "" data_rows
        in
        
        let new_numbers = 
          if number_str <> "" then
            int_of_string number_str :: numbers
          else if numbers <> [] then
            []
          else
            numbers
        in
        
        let new_operation =
          if i < String.length last_row && last_row.[i] <> ' ' then
            last_row.[i]
          else
            operation
        in
        
        let new_acc = 
          if number_str = "" && numbers <> [] then
            (List.rev numbers, operation) :: acc
          else
            acc
        in
        
        process_position (i + 1) new_numbers new_operation new_acc
    in
    
    process_position 0 [] ' ' []

let solve_two columns =
  Array.fold_left (fun acc (numbers, operation) ->
    match operation with
    | '+' -> acc + List.fold_left (+) 0 numbers
    | '*' -> acc + List.fold_left ( * ) 1 numbers
    | _ -> acc
  ) 0 columns



module Solver : Solver = struct
  let part1 lines = 
    let columns = parse_lines_one lines in
    solve_one columns |> string_of_int
  
  let part2 lines = 
    let columns = parse_lines_two lines in
    solve_two columns |> string_of_int
end

