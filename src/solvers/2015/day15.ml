open Solvers.Signature


let get_score i j k l ingredients calories_req =
  let (cap1, dur1, fla1, tex1, cal1) = List.nth ingredients 0 in
  let (cap2, dur2, fla2, tex2, cal2) = List.nth ingredients 1 in
  let (cap3, dur3, fla3, tex3, cal3) = List.nth ingredients 2 in
  let (cap4, dur4, fla4, tex4, cal4) = List.nth ingredients 3 in

  let cap = max 0 (i * cap1 + j * cap2 + k * cap3 + l * cap4) in
  let dur = max 0 (i * dur1 + j * dur2 + k * dur3 + l * dur4) in
  let fla = max 0 (i * fla1 + j * fla2 + k * fla3 + l * fla4) in
  let tex = max 0 (i * tex1 + j * tex2 + k * tex3 + l * tex4) in
  let cal = i * cal1 + j * cal2 + k * cal3 + l * cal4 in

  if calories_req && cal <> 500 then 0
  else cap * dur * fla * tex

let get_best_score total_spoons ingredients calories_req =
  let rec amounts_aux i j k best =
    if i = (total_spoons - 2) then best
    else if j = (total_spoons - i -1) then amounts_aux (i + 1) 1 1 best
    else if k = (total_spoons - i - j) then amounts_aux i (j + 1) 1 best
    else
      let l = total_spoons - i - j - k in
      let this_case = get_score i j k l ingredients calories_req in
      let new_best = if this_case > best then this_case else best in
      amounts_aux i j (k + 1) new_best
  in
  amounts_aux 1 1 1 0

module Solver : Solver = struct

  let part1 _ =
    (* didn't wanna parse it again.. *)
    let data = [(2, 0, -2, 0, 3); (0, 5, -3, 0, 3); (0, 0, 5, -1, 8); (0, -1, 0, 5, 8)] in
    let total_spoons = 100 in
    get_best_score total_spoons data false |> string_of_int


  let part2 _ =
    (* didn't wanna parse it again.. *)
    let data = [(2, 0, -2, 0, 3); (0, 5, -3, 0, 3); (0, 0, 5, -1, 8); (0, -1, 0, 5, 8)] in
    let total_spoons = 100 in
    get_best_score total_spoons data true |> string_of_int
end