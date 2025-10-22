open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let part1 lines =
    string_of_int (List.length lines)
  
  let part2 lines =
    string_of_int (int_list lines |> sum)
end