open Solvers.Signature


let total_interval_size intervals =
  let marked_borders = List.flatten (
    List.map (fun (start_i, end_i) -> [ (start_i, 1); (end_i+1, -1) ]) intervals
  ) in
  let sorted_borders = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) marked_borders in
  let rec aux current_count last_i total = function
    | [] -> total
    | (i, mark) :: rest ->
        let new_total = 
          if current_count > 0 then
            total + (i - last_i)
          else
            total
        in
        aux (current_count + mark) i new_total rest
  in
  aux 0 0 0 sorted_borders



let check_values intervals targets = 
  List.filter ( fun target ->
    List.exists (fun (start_i, end_i) ->
      target >= start_i && target <= end_i
    ) intervals
  ) targets |> List.length


let parse_lines lines = 
  let parse_interval line =
    match String.split_on_char '-' line with
    | [start_s; end_s] -> (int_of_string start_s, int_of_string end_s)
    | _ -> failwith "Invalid interval format"
  in 
  let rec aux intervals = function
    | [] -> failwith "No target numbers"
    | "" :: rest -> (List.rev intervals, List.map int_of_string rest)
    | line :: rest -> aux (parse_interval line :: intervals) rest
  in
  aux [] lines



module Solver : Solver = struct
  let part1 lines = 
    let intervals, targets = parse_lines lines in
    check_values intervals targets |> string_of_int
  
  let part2 lines = 
    let intervals, _ = parse_lines lines in
    total_interval_size intervals |> string_of_int
end

