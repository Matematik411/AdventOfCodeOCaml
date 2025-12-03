open Solvers.Signature
open Utils.String_utils

let most_batteries line total_digits =
  let rec find_batteries batteries_left line_left =
    if batteries_left = 0 then ""
    
    else
      (* let chars_with_idx = String.to_seqi line_left |> List.of_seq in *)
      let chars_with_idx = list_with_indices_of_string line_left in

      let valid_chars = List.filter (fun (i, _) -> 
        String.length line_left - i >= batteries_left
      ) chars_with_idx in

      let (best_i, best_d) = List.fold_left (fun (max_i, max_d) (i, c) ->
        if Char.code c > Char.code max_d then (i, c)
        else (max_i, max_d)
      ) (0, '0') valid_chars in
      
      let remaining = String.sub line_left (best_i + 1) (String.length line_left - best_i - 1) in
      String.make 1 best_d ^ find_batteries (batteries_left - 1) remaining
  in
  find_batteries total_digits line

let count_batteries_i lines_with_indices digits = 
  List.fold_left (fun acc line ->
    let largest_subnumber = most_batteries line digits in
    acc + (int_of_string largest_subnumber)
  ) 0 lines_with_indices

let count_batteries lines digits = 
  List.fold_left (fun acc line ->
    let largest_subnumber = most_batteries line digits in
    acc + (int_of_string largest_subnumber)
  ) 0 lines


module Solver : Solver = struct
  let part1 lines =
    count_batteries lines 2 |> string_of_int
  
  let part2 lines = 
    count_batteries lines 12 |> string_of_int

end

