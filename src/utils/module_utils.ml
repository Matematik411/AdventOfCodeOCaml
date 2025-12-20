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

module type UNIONFIND = sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> unit
  val groups : t -> (int * int list) list
  val groups_sizes : t -> (int * int) list
end

module UnionFind : UNIONFIND = struct
  type t = int array

  let create size =
    Array.init size (fun i -> i)

  let find uf x =
    let rec aux x =
      if uf.(x) = x then x
      else 
        let root = aux uf.(x) in
        uf.(x) <- root;
        root
    in
    aux x

  
  let union uf x y =
    let root_x = find uf x in
    let root_y = find uf y in
    if root_x <> root_y then
      uf.(root_x) <- root_y
  
  let groups uf = (* this is one-time checking at the end *)
    let rec aux i acc =
      if i >= Array.length uf then acc
      else
        let root = find uf i in
        let updated_acc =
          match List.assoc_opt root acc with
          | Some lst -> (root, i :: lst) :: List.remove_assoc root acc
          | None -> (root, [i]) :: acc
        in
        aux (i + 1) updated_acc
    in
    aux 0 []

  let groups_sizes uf = (* this is one-time checking at the end *)
    let rec aux i acc =
      if i >= Array.length uf then acc
      else
        let root = find uf i in
        let updated_acc =
          match List.assoc_opt root acc with
          | Some lst -> (root, lst + 1) :: List.remove_assoc root acc
          | None -> (root, 1) :: acc
        in
        aux (i + 1) updated_acc
    in
    aux 0 []

end