open Solvers.Signature

let count_ways total_capacity all_containers = 
  let memo_table = Hashtbl.create 10000 in
  let rec aux capacity containers =
    let key = (capacity, containers) in
    if Hashtbl.mem memo_table key then
      Hashtbl.find memo_table key
    else
      let result =
        if capacity = 0 then 1
        else if capacity < 0 then 0
        else
          match containers with
          | [] -> 0
          | c :: cs ->
              aux (capacity - c) cs + aux capacity cs 
      in
      Hashtbl.add memo_table key result;
      result
  in
  aux total_capacity all_containers

(* with no memo *)
let count_only_best_ways total_capacity all_containers = 
  let rec aux minimal minimal_count capacity containers number_used =
    if capacity = 0 then (
      if number_used < minimal then (number_used, 1)
      else if number_used = minimal then (minimal, minimal_count + 1)
      else (minimal, minimal_count)
    )
    else if capacity < 0 then (max_int, 0)
    else
      match containers with
      | [] -> (max_int, 0)
      | c :: cs ->
          let (min1, count1) = aux minimal minimal_count (capacity - c) cs (number_used + 1) in
          let (min2, count2) = aux minimal minimal_count capacity cs number_used in
          if min1 < min2 then (min1, count1)
          else if min2 < min1 then (min2, count2)
          else (min1, count1 + count2)
  in
  aux max_int 0 total_capacity all_containers 0


(* lines is of form: ["11", "30", "4", ...]
parse lines into a int list*)
let parse_line lines =
  List.map int_of_string lines


module Solver : Solver = struct

  let part1 lines =
    let containers = parse_line lines in
    count_ways 150 containers |> string_of_int

  let part2 lines =
    let containers = parse_line lines in
    let result = count_only_best_ways 150 containers in
    let () = print_endline ("Minimal number of containers: " ^ (string_of_int (fst result)) ^ "\nNumber of ways: " ^ (string_of_int (snd result))) in
    snd result |> string_of_int
end