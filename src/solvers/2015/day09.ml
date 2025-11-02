open Solvers.Signature
open Utils.List_utils

let parse lines = 
  let parser l =
    let parts = String.split_on_char ' ' l in
    match parts with
      | from :: "to" :: to_ :: "=" :: dist_str :: [] ->
          let dist = int_of_string dist_str in
          (from, to_, dist)
      | _ -> failwith "Bad line format"
  in
  let update_towns t_list t =
    if List.mem t t_list then t_list
    else t :: t_list
  in
  let rec format towns edges = function
    | [] -> towns, edges
    | line :: rest ->
        let (_from, _to, dist) = parser line
        in
        let t1 = update_towns towns _from
        in
        let t2 = update_towns t1 _to
        in
        format t2 (((_from, _to), dist) :: ((_to, _from), dist) :: edges) rest
  in
  format [] [] lines

let dist a b edges = 
  match List.assoc_opt (a, b) edges with
    | Some d -> d
    | None -> 100_000

let dist_part2 a b edges = 
  match List.assoc_opt (a, b) edges with
    | Some d -> d
    | None -> -100_000

let find_cheapest ts es =
  let all_possible = permutations ts in
  let rec path_price cost = function
    | [] | [_] -> cost
    | t1 :: t2 :: rest -> path_price (cost + (dist t1 t2 es)) (t2 :: rest)
  in
  let rec check_paths best = function
    | [] -> best
    | p :: rest -> (
      let price = path_price 0 p in
      if price < best then (check_paths price rest)
      else (check_paths best rest)
    )
  in 
  check_paths max_int all_possible

let find_expensive ts es =
  let all_possible = permutations ts in
  let rec path_price cost = function
    | [] | [_] -> cost
    | t1 :: t2 :: rest -> path_price (cost + (dist_part2 t1 t2 es)) (t2 :: rest)
  in
  let rec check_paths best = function
    | [] -> best
    | p :: rest -> (
      let price = path_price 0 p in
      if price > best then (check_paths price rest)
      else (check_paths best rest)
    )
  in 
  check_paths min_int all_possible

module Solver : Solver = struct
  let part1 lines =
    let towns, edges = parse lines in
    let res = find_cheapest towns edges in
    string_of_int res
    
  let part2 lines =
    let towns, edges = parse lines in
    let res = find_expensive towns edges in
    string_of_int res

end