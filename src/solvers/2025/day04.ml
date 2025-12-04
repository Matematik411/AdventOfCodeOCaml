open Solvers.Signature

let dirs = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]

let parse_grid lines =
  lines
  |> List.map (fun line -> String.to_seq line |> Array.of_seq)
  |> Array.of_list

let can_be_removed grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  
  Array.fold_left (fun acc_j (j, row) ->
    let row_removable = Array.fold_left (fun acc_i (i, cell) ->
      if cell = '@' then
        let neighbors = List.fold_left (fun count (dy, dx) ->
          let new_j = j + dy in
          let new_i = i + dx in
          if new_j >= 0 && new_j < height && new_i >= 0 && new_i < width then
            if grid.(new_j).(new_i) = '@' then count + 1 else count
          else count
        ) 0 dirs in
        
        if neighbors < 4 then (j, i) :: acc_i else acc_i
      else acc_i
    ) [] (Array.mapi (fun i c -> (i, c)) row) in
    row_removable @ acc_j
  ) [] (Array.mapi (fun j r -> (j, r)) grid)

let remove_positions grid removable =
  Array.mapi (fun j row ->
    Array.mapi (fun i cell ->
      if List.mem (j, i) removable then '.' else cell
    ) row
  ) grid

let rec solve grid part total =
  let removable = can_be_removed grid in
  let new_total = total + List.length removable in
  
  if (part = 1 || List.length removable = 0) then new_total
  else
    let new_grid = remove_positions grid removable in
    solve new_grid part new_total

module Solver : Solver = struct
  let part1 lines = 
    let grid = parse_grid lines in
    solve grid 1 0 |> string_of_int
  
  let part2 lines = 
    let grid = parse_grid lines in
    solve grid 2 0 |> string_of_int

end

