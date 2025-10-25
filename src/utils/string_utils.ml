let list_of_string str =
  let rec aux i acc =
    if i >= String.length str then List.rev acc
    else aux (i + 1) (String.get str i :: acc)
  in
  aux 0 []

let str_split c str =
  String.split_on_char c str

let str_split_int c str =
  List.map int_of_string (str_split c str)

let count_letters str =
  str
  |> String.to_seq
  |> Seq.fold_left (fun acc c ->
       match List.assoc_opt c acc with
       | Some count -> (c, count + 1) :: List.remove_assoc c acc
       | None -> (c, 1) :: acc
     ) []
  |> List.rev