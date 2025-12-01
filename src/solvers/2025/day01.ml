open Solvers.Signature

let move_pointer instructions =
  let rec aux score current_pos = function
    | [] -> score
    | n :: rest ->
        let new_pos = ((current_pos + n) mod 100 + 100) mod 100
        in
        if new_pos = 0 then
          aux (score + 1) new_pos rest
        else
          aux score new_pos rest
  in
  aux 0 50 instructions

(* for part 2 change instructions into a list of 1 and -1 *)
let change_instructions instructions =
  List.concat_map (fun n ->
    List.init (abs n) (fun _ -> if n < 0 then -1 else 1)
  ) instructions

let parse_lines lines = 
  let rec aux acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let dir = String.get line 0 in
        let d = String.length line in
        let n = int_of_string (String.sub line 1 (d - 1)) in
        match dir with
        | 'L' -> aux (-n :: acc) rest
        | 'R' -> aux (n :: acc) rest
        | _ -> failwith "Invalid direction"
  in
  aux [] lines

module Solver : Solver = struct
  let part1 lines =
    let instructions = parse_lines lines in
    string_of_int (move_pointer instructions)
  
  let part2 lines =
    let instructions = parse_lines lines in
    let changed_instructions = change_instructions instructions in
    string_of_int (move_pointer changed_instructions)
end

