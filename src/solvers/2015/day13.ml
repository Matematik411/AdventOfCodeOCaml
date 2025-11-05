open Solvers.Signature
open Utils.String_utils
open Utils.List_utils

let calculate_happiness seating happiness_map =
  let array_of_seating = Array.of_list seating in
  let d = Array.length array_of_seating in
  let rec aux acc = function
    | i when i = d -> acc
    | i ->
        let left = array_of_seating.((i - 1 + d) mod d) in
        let right = array_of_seating.((i + 1) mod d) in
        let current = array_of_seating.(i) in
        let acc' = acc + (List.assoc (current, left) happiness_map) + (List.assoc (current, right) happiness_map) in
        aux acc' (i + 1)
  in
  aux 0 0

let check_all_seatings people values =
  let all_permutations = permutations people in
  List.fold_left (fun acc perm ->
    let happiness = calculate_happiness perm values in
    max acc happiness
  ) 0 all_permutations


let my_pairs other_people =
  let rec aux acc = function
    | [] -> acc
    | p :: rest -> aux (((p, "me"), 0) :: (("me", p), 0) :: acc) rest
  in
  aux [] other_people


let rec list_all_people people = function
  | [] -> people
  | ((p1, p2), _) :: rest ->
      let people' = if List.mem p1 people then people else p1 :: people in
      let people'' = if List.mem p2 people' then people' else p2 :: people' in
      list_all_people people'' rest


let parse_line lines =
  let parse_line line =
    let words = str_split ' ' line in
    let person1 = List.nth words 0 in
    let gain_or_lose = List.nth words 2 in
    let amount = int_of_string (List.nth words 3) in
    let person2 = String.sub (List.nth words 10) 0 ((String.length (List.nth words 10)) - 1) in
    let happiness =
      if gain_or_lose = "gain" then amount
      else -amount
    in
    ((person1, person2), happiness)
  in
  List.map parse_line lines

module Solver : Solver = struct

  let part1 lines =
    let happiness_list = parse_line lines in
    let all_people = list_all_people [] happiness_list in
    check_all_seatings all_people happiness_list |> string_of_int

  let part2 lines =
    let happiness_list = parse_line lines in
    let other_people = list_all_people [] happiness_list in
    let my_happiness = my_pairs other_people in
    check_all_seatings ("me" :: other_people) (my_happiness @ happiness_list) |> string_of_int

end