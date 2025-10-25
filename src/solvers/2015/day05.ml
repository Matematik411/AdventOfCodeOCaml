open Solvers.Signature
open Utils.String_utils

let three_vowels chars_list =
  let vowels = ['a'; 'e'; 'i'; 'o'; 'u'] in
  List.filter (fun c -> List.mem c vowels) chars_list |> List.length >= 3

let following_chars chars_list =
  let banned_pairs = ["ab"; "cd"; "pq"; "xy"] in
  let rec rules_check double_found = function
    | [] | [_] -> double_found
    | x :: y :: xs ->
        let pair = String.make 1 x ^ String.make 1 y in
        if List.mem pair banned_pairs then false
        else
          let new_double_found = double_found || (x = y) in
          rules_check new_double_found (y :: xs)
  in
  rules_check false chars_list

let pairs_of_chars chars_list =
  let rec aux acc = function
    | [] | [_] -> acc
    | x :: y :: xs ->
        let pair = String.make 1 x ^ String.make 1 y in
        aux (pair :: acc) (y :: xs)
  in
  aux [] chars_list

let rec repeat_not_consecutive = function
  | [] | [_] -> false
  | p :: q :: rest ->
      if List.mem p rest then true
      else repeat_not_consecutive (q :: rest)

let axa_pattern chars_list =
  let rec aux = function
    | x :: _ :: z :: _ when x = z -> true
    | _ :: xs -> aux xs
    | _ -> false
  in
  aux chars_list


module Solver : Solver = struct
  let part1 lines =
    List.filter (fun line ->
      let chars_list = list_of_string line in
      three_vowels chars_list && following_chars chars_list
    ) lines 
    |> List.length 
    |> string_of_int
  
  let part2 lines =
    List.filter (fun line ->
      let chars_list = list_of_string line in
      repeat_not_consecutive (pairs_of_chars chars_list) && axa_pattern chars_list
    ) lines
    |> List.length
    |> string_of_int
end