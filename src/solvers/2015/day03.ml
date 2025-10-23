open Solvers.Signature
open Utils.List_utils
open Utils.String_utils
open Utils.Module_utils

module Coords = Set.Make(IntPair)

module Santa = struct
  type t = {
    x: int;
    y: int;
  }

  let move santa direction =
    match direction with
    | "^" -> { santa with y = santa.y + 1 }
    | ">" -> { santa with x = santa.x + 1 }
    | "v" -> { santa with y = santa.y - 1 }
    | "<" -> { santa with x = santa.x - 1 }
    | _ -> failwith "Invalid move"
  
  let update_visited visited santa =
    Coords.add (santa.x, santa.y) visited
end

let one_santa moves_all =
  let rec steps santa visited = function
    | m :: ms -> (
        let new_santa = Santa.move santa m in
        steps new_santa (Santa.update_visited visited new_santa) ms
    )
    | _ -> visited
  in
  steps ({Santa.x = 0; y = 0}) (Coords.add (0,0) (Coords.empty)) moves_all
  |> Coords.cardinal

let two_santas moves_all =
  let rec steps moving on_break visited = function
    | m :: ms -> (
        let after_move = Santa.move moving m in 
        steps on_break after_move (Santa.update_visited visited after_move) ms
    )
    | _ -> visited
  in
  steps ({Santa.x = 0; y = 0}) ({Santa.x = 0; y = 0}) (Coords.add (0,0) (Coords.empty)) moves_all
  |> Coords.cardinal
  
module Solver : Solver = struct
  let part1 lines =
    let moves_list = list_of_string (first_element lines) in
    one_santa moves_list |> string_of_int
    
  let part2 lines =
    let moves_list = list_of_string (first_element lines) in
    two_santas moves_list |> string_of_int
end