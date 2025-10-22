let read_lines filename =
  let ic = open_in filename in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_all []

let print_in_file filename text =
  let chan = open_out filename in
  output_string chan (text ^ "\n");
  close_out chan