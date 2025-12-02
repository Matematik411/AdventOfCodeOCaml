open Solvers.Signature
open Utils.String_utils


let invalids bounds_list part =
  List.fold_left (fun acc (min_b, max_b) ->
    acc + 
    
    List.fold_left ( + ) 0 ((List.filter (fun n -> 
      let str_n = string_of_int n in
      let d = String.length str_n in
      let delitve = 
        if part = 1 then 2
        else d
      in
      List.exists (fun i ->
        if d mod i = 0 then
          let pattern = String.sub str_n 0 (d / i) in
          let repeated = String.concat "" (List.init i (fun _ -> pattern)) in
          repeated = str_n
        else
          false
      ) (List.init (delitve-1) (fun i -> i + 2)))) (* 2 -> d (both included) *)
      
      (List.init (max_b - min_b + 1) (fun i -> min_b + i)))
    ) 0 bounds_list


module Solver : Solver = struct
  let part1 lines =
    let first_line = List.hd lines in
    let intervals = str_split ',' first_line in
    let bounds = 
      List.map (fun interval ->
        match str_split '-' interval with
        | [min_str; max_str] -> (int_of_string min_str, int_of_string max_str)
        | _ -> failwith "Invalid interval format"
      ) intervals in 
    string_of_int (invalids bounds 1)
  
  let part2 lines =
    let first_line = List.hd lines in
    let intervals = str_split ',' first_line in
    let bounds = 
      List.map (fun interval ->
        match str_split '-' interval with
        | [min_str; max_str] -> (int_of_string min_str, int_of_string max_str)
        | _ -> failwith "Invalid interval format"
      ) intervals in 
    string_of_int (invalids bounds 2)
end

