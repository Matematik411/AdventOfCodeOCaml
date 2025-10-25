open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

let part1_stairs brackets =
  let rec moving_floors floor = function
    | x :: xs when x = '(' -> moving_floors (floor + 1) xs
    | x :: xs when x = ')' -> moving_floors (floor - 1) xs
    | _ -> floor
  in 
  moving_floors 0 brackets
  
let part2_stairs brackets =
  let rec moving_floors floor step = function
    | x :: xs when x = '(' -> moving_floors (floor + 1) (step + 1) xs
    | x :: xs when x = ')' -> if floor = 0 then step else moving_floors (floor - 1) (step + 1) xs
    | _ -> floor
  in 
  moving_floors 0 1 brackets

module Solver : Solver = struct
  let part1 lines =
    let brackets_list = list_of_string (first_element lines) in
    part1_stairs brackets_list |> string_of_int
    
  let part2 lines =
    let brackets_list = list_of_string (first_element lines) in
    part2_stairs brackets_list |> string_of_int
    
end