open Solvers.Signature
open Utils.Module_utils
open Utils.String_utils

module Coords = Set.Make(IntPair)

let parse_splitters lines =
  let rec aux found row = function
    | [] -> found
    | line :: rest ->
        let indices = List.filter_map (
          fun (i, ch) -> if ch = '^' then Some (row, i) else None
        ) (List.mapi (fun i c -> (i, c)) (list_of_string line)) 
        in
        aux (found @ indices) (row + 1) rest
  in
  aux [] 0 lines

let count_splits start_x splitters =
  (* get splitters in correct order *)
  let rec aux active_beams total = function
    | [] -> total
    | (y, x) :: rest ->
        if Coords.exists (fun (y', x') -> x' = x && y' >= 0 && y' < y) active_beams then
          aux (
            active_beams 
            |> Coords.filter (fun (y', x') -> not (x' = x && y' < y))
            |> Coords.add (y + 1, x - 1) 
            |> Coords.add (y + 1, x + 1)
          ) (total + 1) rest
        else
          aux active_beams total rest
  in
  aux (Coords.singleton (1, start_x)) 0 splitters

let parse_to_drawing_array lines =
  let drawing_array = 
    Array.of_list (List.map (fun line -> Array.of_list (list_of_string_strings line)) lines)
  in
  drawing_array



let count_realities drawing_array start_x = 
  let height = Array.length drawing_array in
  let width = Array.length drawing_array.(0) in

  let rec aux x y =
    if x < 0 then aux (width - 1) (y - 1)
    else if y < 0 then ()
    else if y = (height - 1) then (drawing_array.(y).(x) <- "1"; aux (x - 1) y)
    else if drawing_array.(y).(x) = "." || drawing_array.(y).(x) = "S" then
      (drawing_array.(y).(x) <- drawing_array.(y + 1).(x); aux (x - 1) y)
    else if drawing_array.(y).(x) = "^" then
      (drawing_array.(y).(x) <- 
        (let left = drawing_array.(y + 1).(x - 1) in
        let right = drawing_array.(y + 1).(x + 1) in
        string_of_int ((int_of_string left) + (int_of_string right)));
        aux (x - 1) y)
    else
      failwith "Unexpected character in drawing array";
  in
  aux (width - 1) (height - 1);
  int_of_string drawing_array.(0).(start_x)

module Solver : Solver = struct
  let part1 lines = 
    let start_x = String.index (List.hd lines) 'S' in
    let splitters = parse_splitters lines in
    count_splits start_x splitters |> string_of_int
  
  let part2 lines =
    let start_x = String.index (List.hd lines) 'S' in
    count_realities (parse_to_drawing_array lines) start_x |> string_of_int
end

