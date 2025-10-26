module IntPair = struct
  type t = int * int
  let compare = compare  
end

module PriorityQ = struct
  type 'a t = (int * 'a) list
  
  let empty = []
  
  let insert pq priority item =
    let rec ins = function
      | [] -> [(priority, item)]
      | (p, x) :: rest as l ->
          if priority < p then (priority, item) :: l
          else (p, x) :: ins rest
    in
    ins pq
  
  let extract_min = function
    | [] -> None
    | (p, x) :: rest -> Some ((p, x), rest)
end