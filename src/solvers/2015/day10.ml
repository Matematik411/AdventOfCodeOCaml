open Solvers.Signature
open Utils.String_utils

let look_and_say pattern = 
  let buf = Buffer.create (String.length pattern * 2) in
  let rec aux n k i =
    if i >= String.length pattern then (
      Buffer.add_char buf (digit_to_char k);
      Buffer.add_char buf (digit_to_char n))
    else 
      let curr_char = char_to_digit pattern.[i] in
      if curr_char = n then
        aux n (k + 1) (i + 1)
      else begin
        Buffer.add_char buf (digit_to_char k);
        Buffer.add_char buf (digit_to_char n);
        aux curr_char 1 (i + 1)
      end
  in
  if String.length pattern > 0 then
    aux (char_to_digit pattern.[0]) 0 0;
  Buffer.contents buf


let loop loops start_data =
  let rec aux acc = function
    | 0 -> String.length acc
    | i -> aux (look_and_say acc) (i - 1)
  in
  aux start_data loops


module Solver : Solver = struct
  let part1 _ =
    let my_input = "1113122113" in
    my_input |> loop 40 |> string_of_int (* 0.016 sec *)
    
  let part2 _ =
    let my_input = "1113122113" in
    my_input |> loop 50 |> string_of_int (* 0.171 sec *)

end