open Solvers.Signature
open Utils.String_utils

let wrapping_paper values = 
  match values with
  | [a; b; c] ->  2 * (a * b + b * c + c * a) + a * b
  | _ -> failwith "Invalid dimensions"

let ribbon values =
  match values with
  | [a; b; c] -> 2 * (a + b) + (a * b * c)
  | _ -> failwith "Invalid dimensions"

module Solver : Solver = struct
  let part1 lines =
    let rec loop total = function
      | l :: rest -> let dims = str_split_int 'x' l in
                     let sorted_dims = List.sort compare dims in
                     loop (total + (wrapping_paper sorted_dims)) rest
      | [] -> total
    in  
    string_of_int (loop 0 lines)
  
  let part2 lines =
    let rec loop total = function
      | l :: rest -> let dims = str_split_int 'x' l in
                     let sorted_dims = List.sort compare dims in
                     loop (total + (ribbon sorted_dims)) rest
      | [] -> total
    in
    string_of_int (loop 0 lines)
end