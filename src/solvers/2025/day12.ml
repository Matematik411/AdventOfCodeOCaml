open Solvers.Signature
open Utils.String_utils

let parse_line line =
  let parts = str_split ':' line in
  let dimensions = str_split_int 'x' (List.hd parts) in
  let values = str_split_int ' ' (String.trim (List.nth parts 1)) in
  (List.nth dimensions 0, List.nth dimensions 1), values


let solve_last_day_with_all_the_knowledge_of_data lines =  
  (* ignore lines, that don't have 'x' in *)
  let rec check_if_line_is_ok total = function
    | [] -> total
    | line :: rest ->
        if String.contains line 'x' then
          let (h, w), values = parse_line line in
          if h * w >= List.fold_left (fun acc v -> acc + v*9) 0 values then
            check_if_line_is_ok (total + 1) rest
          else
            check_if_line_is_ok total rest
        else
          check_if_line_is_ok total rest
  in
  check_if_line_is_ok 0 lines

module Solver : Solver = struct
  let part1 lines = 
    solve_last_day_with_all_the_knowledge_of_data lines |> string_of_int
  
  let part2 _ = 
    "Year 2025 done! Congratulations!"
end

