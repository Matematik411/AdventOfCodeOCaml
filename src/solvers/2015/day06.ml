open Solvers.Signature
open Utils.Array_utils

type action = TurnOn | TurnOff | Toggle
type coord = int * int
type instruction = action * coord * coord

let parse_coord s =
  Scanf.sscanf s "%d,%d" (fun x y -> (x, y))
  
let parse_instruction line =
  let parts = String.split_on_char ' ' line in
  match parts with
  | "turn" :: "on" :: from :: "through" :: to_ :: [] ->
      (TurnOn, parse_coord from, parse_coord to_)
  | "turn" :: "off" :: from :: "through" :: to_ :: [] ->
      (TurnOff, parse_coord from, parse_coord to_)
  | "toggle" :: from :: "through" :: to_ :: [] ->
      (Toggle, parse_coord from, parse_coord to_)
  | _ -> failwith "Bad instruction format"

let apply_to_range grid action (x1, y1) (x2, y2) =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid.(y).(x) <- match action with
        | TurnOn -> true
        | TurnOff -> false
        | Toggle -> not grid.(y).(x)
    done
  done

let apply_to_range_brightness grid action (x1, y1) (x2, y2) =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid.(y).(x) <- match action with
        | TurnOn -> grid.(y).(x) + 1
        | TurnOff -> max 0 (grid.(y).(x) - 1)
        | Toggle -> grid.(y).(x) + 2
    done
  done

module Solver : Solver = struct
  let part1 lines =
    let part_one_grid = create_2d_array 1000 1000 false in
    let instructions = List.map parse_instruction lines in
    List.iter 
      (fun (action, top_left, bottom_right) -> apply_to_range part_one_grid action top_left bottom_right)
      instructions;
    count_2d_bool part_one_grid |> string_of_int
  
  let part2 lines =
    let part_two_grid = create_2d_array 1000 1000 0 in
    let instructions = List.map parse_instruction lines in
    List.iter 
      (fun (action, top_left, bottom_right) -> apply_to_range_brightness part_two_grid action top_left bottom_right)
      instructions;
    count_2d_int part_two_grid |> string_of_int
end