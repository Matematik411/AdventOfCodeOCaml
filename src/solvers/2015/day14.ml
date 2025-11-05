open Solvers.Signature
open Utils.String_utils


let distance_after_time t (_, speed, fly_time, rest_time) =
  let cycle_time = fly_time + rest_time in
  let full_cycles = t / cycle_time in
  let remaining_time = t mod cycle_time in
  let distance_in_full_cycles = full_cycles * speed * fly_time in
  let distance_in_remaining_time =
    if remaining_time >= fly_time then
      speed * fly_time
    else
      speed * remaining_time
  in
  distance_in_full_cycles + distance_in_remaining_time

let max_distance_after_time t reindeers =
  List.fold_right
    (fun r m -> max (distance_after_time t r) m)
    reindeers
    0

let scoring_after_each_second t reindeers =
  let times_to_t = List.init t (fun x -> x+1) in
  let max_dists = List.map (fun time -> (time, max_distance_after_time time reindeers)) times_to_t in

  let points_for_reindeer r =
    List.fold_right
      (fun (t, max_d) score -> if (distance_after_time t r = max_d) then score + 1 else score)
      max_dists
      0
  in

  List.fold_right
    (fun r m -> max (points_for_reindeer r) m)
    reindeers
    0


let parse_line lines =
  let parse_line line =
    let words = str_split ' ' line in
    let speed = int_of_string (List.nth words 3) in
    let time = int_of_string (List.nth words 6) in
    let resting = int_of_string (List.nth words 13) in
    (0, speed, time, resting) (* zero is set up for part 2 *)
  in
  List.map parse_line lines

module Solver : Solver = struct

  let part1 lines =
    let data = parse_line lines in
    max_distance_after_time 2503 data |> string_of_int

  let part2 lines =
    let data = parse_line lines in
    scoring_after_each_second 2503 data |> string_of_int
end