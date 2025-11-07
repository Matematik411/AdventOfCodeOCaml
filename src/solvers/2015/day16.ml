open Solvers.Signature
open Utils.List_utils
open Utils.String_utils


let is_a_match present_from auntie_compounds =
  List.for_all
    (fun (name, value) ->
      match List.assoc_opt name auntie_compounds with
      | Some v -> v = value
      | None -> true)
    present_from


let find_specific_auntie aunties present_from =
  List.fold_right
    (fun (sue_id, compounds) acc ->
      if is_a_match present_from compounds then sue_id else acc)
    aunties
    (-1)

(* part 2, very similar *)
let satisfies_conditions present_from auntie_compounds =
  List.for_all
    (fun (name, value) ->
      match List.assoc_opt name auntie_compounds with
      | Some v when name = "cats" || name = "trees" -> v > value
      | Some v when name = "pomeranians" || name = "goldfish" -> v < value
      | Some v -> v = value
      | None -> true)
    present_from


let find_specific_auntie_part2 aunties present_from =
  List.fold_right
    (fun (sue_id, compounds) acc ->
      if satisfies_conditions present_from compounds then sue_id else acc)
    aunties
    (-1)



(* let compounds = ["children"; "cats"; "samoyeds"; "pomeranians"; "akitas";
                 "vizslas"; "goldfish"; "trees"; "cars"; "perfumes"] in *)
                 
let remove_colon s = String.concat "" (String.split_on_char ':' s)
(* Line: "Sue 5: goldfish: 1, trees: 3, perfumes: 10"
   -> (5, [("goldfish", 1); ("trees", 3); ("perfumes", 10)]) 
  For this, split by " ", remove first 2 words, join other things back and then split by ","
   *) 
let parse_line lines =
  let parse_line line =
    let words = str_split ' ' line in
    let sue_id = List.nth words 1 |> remove_colon |> int_of_string in
    let rest = String.concat " " (list_drop 2 words) in
    let compounds = str_split ',' rest |> List.map (fun s ->
      let parts = str_split ':' s in
      let name = String.trim (List.nth parts 0) in
      let value = int_of_string (String.trim (List.nth parts 1)) in
      (name, value)
    ) in
    (sue_id, compounds)
  in
  List.map parse_line lines

module Solver : Solver = struct

  let part1 lines =
    let aunties = parse_line lines in
    let present_from = [("children", 3); ("cats", 7); ("samoyeds", 2);
                        ("pomeranians", 3); ("akitas", 0); ("vizslas", 0);
                        ("goldfish", 5); ("trees", 3); ("cars", 2); ("perfumes", 1)] in
    find_specific_auntie aunties present_from |> string_of_int

  let part2 lines =
    let aunties = parse_line lines in
    let present_from = [("children", 3); ("cats", 7); ("samoyeds", 2);
                        ("pomeranians", 3); ("akitas", 0); ("vizslas", 0);
                        ("goldfish", 5); ("trees", 3); ("cars", 2); ("perfumes", 1)] in
    find_specific_auntie_part2 aunties present_from |> string_of_int
end