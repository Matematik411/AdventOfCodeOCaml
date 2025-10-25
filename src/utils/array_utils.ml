let create_2d_array rows cols default =
  Array.init rows (fun _ -> Array.init cols (fun _ -> default))

let count_2d_int int_grid =
  Array.fold_left (fun acc row ->
    acc + (Array.fold_left (+) 0 row)
  ) 0 int_grid

let count_2d_bool bool_grid =
  Array.fold_left (fun acc row ->
    acc + Array.fold_left (fun sum cell -> 
      if cell then sum + 1 else sum
    ) 0 row
  ) 0 bool_grid