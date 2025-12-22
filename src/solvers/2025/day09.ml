open Solvers.Signature
open Utils.String_utils

let check_all_rectangles points =
  List.fold_left ( fun acc1 (x1, y1) ->
    List.fold_left (fun acc2 (x2, y2) ->
      let area = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) in
      max acc2 area
    ) acc1 points
  ) 0 points

(*
input file:
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3 

this is on the grid:
..............
.......#XXX#..
.......X...X..
..#XXXX#...X..
..X........X..
..#XXXXXX#.X..
.........X.X..
.........#X#..
..............

i want make_horizontal_lines to be:
[
(1, (7, 11));
(2, (7, 11));
(3, (2, 11));
(4, (2, 11));
(5, (2, 11));
(6, (9, 11));
(7, (9, 11))
]

*)


(* this loops over all the edges, and creates an  (int * int array) list for each y coordinate saves the leftmost and rightmost x*)
let make_horizontal_lines points =
  let extended_points = 
    match points with
    | [] -> []
    | first :: _ -> points @ [first]
  in
  let rec make_pairs lst = 
    match lst with
    | [] | [_] -> []
    | a :: b :: rest -> (a, b) :: make_pairs (b :: rest)
  in
  let edges = make_pairs extended_points in
  let all_edge_points = List.concat_map (fun ((x1, y1), (x2, y2)) ->
    if x1 = x2 then
      (* vertical edge *)
      List.init (abs (y2 - y1) + 1) (fun i -> (x1, min y1 y2 + i))
    else if y1 = y2 then
      (* horizontal edge *)
      [(x1, y1); (x2, y1)]
    else
      []
  ) edges in
  let tbl = Hashtbl.create (List.length all_edge_points) in
  List.iter (fun (x, y) ->
    match Hashtbl.find_opt tbl y with
    | None -> Hashtbl.add tbl y (x, x)
    | Some (x_min, x_max) ->
        let new_x_min = min x x_min in
        let new_x_max = max x x_max in
        Hashtbl.replace tbl y (new_x_min, new_x_max)
  ) all_edge_points;
  tbl

let is_rectangle_inside horizontal_lines (x1, y1) (x2, y2) =
  let all_y_values = List.init (abs (y2 - y1) + 1) (fun i -> min y1 y2 + i) in
  List.for_all (fun y ->
    let y_horizontal_line = Hashtbl.find_opt horizontal_lines y in
    match y_horizontal_line with
    | None -> false
    | Some (x_start, x_end) ->
        x_start <= min x1 x2 && x_end >= max x1 x2
  ) all_y_values

let check_only_inside_rectangles points =
  let horizontal_lines = make_horizontal_lines points in
  List.fold_left ( fun acc1 (x1, y1) ->
    List.fold_left (fun acc2 (x2, y2) ->
      if is_rectangle_inside horizontal_lines (x1, y1) (x2, y2) then
        let area = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) in
        max acc2 area
      else
        acc2
    ) acc1 points
  ) 0 points



module Solver : Solver = struct
  let part1 lines = 
    let points = List.map (fun line ->
      let coords = str_split_int ',' line in
      (List.nth coords 0, List.nth coords 1)
    ) lines in
    check_all_rectangles points |> string_of_int
  
  let part2 lines = (* takes 430s = 11 mins *)
    let points = List.map (fun line ->
      let coords = str_split_int ',' line in
      (List.nth coords 0, List.nth coords 1)
    ) lines in
    check_only_inside_rectangles points |> string_of_int
end

