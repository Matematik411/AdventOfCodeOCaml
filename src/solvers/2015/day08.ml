open Solvers.Signature
open Utils.String_utils

(* we're looping over the strings, because it's easier than counting chars *)

let reduce_string chars_list =
  let rec parse acc = function
    | [] -> acc 
    | '\\' :: '\"' :: xs  | '\\' :: '\\' :: xs | '\\' :: 'x' :: _ :: _ :: xs -> parse (acc + 1) xs
    | '\"' :: xs -> parse acc xs (* skip first and last quotation *)
    | _ :: xs -> parse (acc + 1) xs
  in
  parse 0 chars_list

let expand_string chars_list =
    let rec parse acc = function
    | [] -> acc 
    | '\\' :: '\"' :: xs  | '\\' :: '\\' :: xs -> parse (acc + 4) xs
    | '\\' :: 'x' :: _ :: _ :: xs -> parse (acc + 5) xs
    | '\"' :: xs -> parse (acc + 3) xs (* account for starting and ending quotation char here *)
    | _ :: xs -> parse (acc + 1) xs
  in
  parse 0 chars_list

module Solver : Solver = struct
  let part1 lines =
    List.fold_left (fun acc line ->
      let code_length = String.length line in
      let shertened_length = reduce_string (list_of_string line) in
      acc + (code_length - shertened_length)
    ) 0 lines
    |> string_of_int
  
  let part2 lines =
    List.fold_left (fun acc line ->
      let expanded_length = expand_string (list_of_string line) in
      let code_length = String.length line in
      acc + (expanded_length - code_length)
    ) 0 lines
    |> string_of_int
end