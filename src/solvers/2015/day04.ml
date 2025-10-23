open Solvers.Signature

let find_advent_coin secret_key leading_zeros =
  let rec try_number n =
    let input = secret_key ^ string_of_int n in
    let hash = Digestif.MD5.(to_hex (digest_string input)) in
    if String.length hash >= leading_zeros && String.sub hash 0 leading_zeros = (String.make leading_zeros '0') then
      n
    else
      try_number (n + 1)
  in
  try_number 1

  module Solver : Solver = struct
  let my_code = "ckczppom"

  let part1 _ =
    find_advent_coin my_code 5 |> string_of_int
    
  let part2 _ =
    find_advent_coin my_code 6 |> string_of_int
    
end