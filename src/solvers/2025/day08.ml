open Solvers.Signature
open Utils.Module_utils
open Utils.String_utils

let euclidean_squared p1 p2 =
  let dx = (List.nth p1 0) - (List.nth p2 0) in
  let dy = (List.nth p1 1) - (List.nth p2 1) in
  let dz = (List.nth p1 2) - (List.nth p2 2) in
  dx * dx + dy * dy + dz * dz


let make_n_edges points connections =
  let dists_sorted = 
    List.mapi (fun i p1 ->
      List.mapi (fun j p2 ->
        if i < j then Some (euclidean_squared p1 p2, (i, j)) else None
      ) points
      |> List.filter_map Fun.id
    ) points |> List.flatten |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)
  in
  let uf = UnionFind.create (List.length points) in
  List.iter (fun (_, (i, j)) ->
    UnionFind.union uf i j
  ) (List.filteri (fun i _ -> i < connections) dists_sorted);
  let groups_sizes = UnionFind.groups_sizes uf |> List.sort (fun (_, s1) (_, s2) -> compare (-s1) (-s2))
  in
  let product_of_top3 =
    match groups_sizes with
    | (_, s1) :: (_, s2) :: (_, s3) :: _ -> s1 * s2 * s3
    | _ -> 0
  in
  product_of_top3

let connect_until_only_one points points_array =
  let dists_sorted = 
    List.mapi (fun i p1 ->
      List.mapi (fun j p2 ->
        if i < j then Some (euclidean_squared p1 p2, (i, j)) else None
      ) points
      |> List.filter_map Fun.id
    ) points |> List.flatten |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)
  in
  let uf = UnionFind.create (List.length points) in
  let rec add_until_one connections_made edges last_val = 
    if connections_made >= (List.length points) - 1 then
      last_val
    else
      match edges with
      | [] -> -1 (* should not happen *)
      | (_, (i, j)) :: rest ->
          let solution_val = points_array.(i).(0) * points_array.(j).(0) in
          let root_i = UnionFind.find uf i in
          let root_j = UnionFind.find uf j in
          if root_i = root_j then
            add_until_one connections_made rest solution_val
          else
            (UnionFind.union uf root_i root_j;
            add_until_one (connections_made + 1) rest solution_val)
  in
  add_until_one 0 dists_sorted (-1)


module Solver : Solver = struct
  let part1 lines = 
    let points = List.map (fun line -> str_split_int ',' line) lines in
    make_n_edges points 1000 |> string_of_int
  
  let part2 lines = 
    let points = List.map (fun line -> str_split_int ',' line) lines in
    let points_array = Array.of_list (List.map (fun p -> Array.of_list p) points) in
    connect_until_only_one points points_array |> string_of_int
end

