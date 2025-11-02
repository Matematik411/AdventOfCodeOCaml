open Solvers.Signature
open Utils.String_utils

let pass_to_int_list string =
  string |> list_of_string |> List.map letter_to_int

let int_list_to_pass i_list =
  i_list |> List.map int_to_letter |> List.to_seq |> String.of_seq

let next_i_list i_list =
  let rec aux acc mode = function
    | [] -> acc
    | i :: rest when (i = 25 && mode = 1) -> aux (0 :: acc) 1 rest
    | i :: rest when mode = 1 -> aux ((i+1) :: acc) 0 rest
    | i :: rest -> aux (i :: acc) mode rest
  in
  aux [] 1 (List.rev i_list)

let rec three_in_a_row = function
  | [] | _ :: [] | _ :: _ :: [] -> false
  | i :: j :: k :: rest -> if (k - j = 1 && j - i = 1) then true else three_in_a_row (j :: k :: rest)

let without_forbidden i_list =
  let forbidden = [letter_to_int 'i'; letter_to_int 'o'; letter_to_int 'l'] in
  let rec aux = function
    | [] -> true
    | i :: _ when List.mem i forbidden -> false
    | _ :: rest -> aux rest
  in
  aux i_list

let non_succ_pairs i_list =
  let rec aux acc = function
    | [] | [_] -> false
    | x :: y :: _ when (x = y && acc) -> true
    | x :: y :: rest when x = y -> aux true rest
    | _ :: rest -> aux acc rest 
  in
  aux false i_list

let find_next_valid password skip_corrects =
  let pass_i_list = pass_to_int_list password in
  let rec check i_list n =
    if three_in_a_row i_list && (without_forbidden i_list && non_succ_pairs i_list) 
      then (
        match n with
          | 0 -> int_list_to_pass i_list
          | n' -> check (next_i_list i_list) (n'-1)
      )
    else check (next_i_list i_list) n
  in
  check pass_i_list skip_corrects


module Solver : Solver = struct
  
  let part1 _ =
    let first_password = "hxbxwxba" in
    find_next_valid first_password 0
    
  
  let part2 _ = 
    let first_password = "hxbxwxba" in
    find_next_valid first_password 1

end