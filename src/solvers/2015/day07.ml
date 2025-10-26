open Solvers.Signature

type wire_table = (string, int option) Hashtbl.t
type gate = 
  | And of string * string * string
  | Or of string * string * string
  | LShift of string * int * string
  | RShift of string * int * string
  | Not of string * string
  | Assign of string * string



module WireComputer = struct
  type t = {
    wires: wire_table;
    all_gates: gate list;
  }

  let add_wire computer gate =
    Hashtbl.replace computer.wires gate (int_of_string_opt gate)
    
  let update_wire computer wire_name value =
    Hashtbl.replace computer.wires wire_name (Some value)


  let evaluate_gate computer gate =
    match gate with
    | And (left, right, target) -> (
        match (Hashtbl.find computer.wires left, Hashtbl.find computer.wires right) with
        | (Some l_val, Some r_val) ->
            update_wire computer target (l_val land r_val)
        | _ -> ()
      ) 
    | Or (left, right, target) -> (
        match (Hashtbl.find computer.wires left, Hashtbl.find computer.wires right) with
        | (Some l_val, Some r_val) ->
            update_wire computer target (l_val lor r_val)
        | _ -> ()
      )
    | LShift (left, shift, target) -> (
        match Hashtbl.find computer.wires left with
        | Some l_val ->
            update_wire computer target (l_val lsl shift)
        | _ -> ()
      )
    | RShift (left, shift, target) -> (
        match Hashtbl.find computer.wires left with
        | Some l_val ->
            update_wire computer target (l_val lsr shift)
        | _ -> ()
      )
    | Not (gate, target) -> (
        match Hashtbl.find computer.wires gate with
        | Some g_val ->
            update_wire computer target (lnot g_val land 0xFFFF)
        | _ -> ()
      )
    | Assign (gate, target) -> (
        match Hashtbl.find computer.wires gate with
        | Some g_val ->
            update_wire computer target g_val
        | _ -> ()
      )

  let process_all_gates computer =
    List.iter (evaluate_gate computer) computer.all_gates
      
  let parse_gate_instructions computer line =
    let instructions = String.split_on_char ' ' line in
    match instructions with
      | left :: "AND" :: right :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer right;
          add_wire computer target;
          { computer with all_gates = And (left, right, target) :: computer.all_gates }
      | left :: "OR" :: right :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer right;
          add_wire computer target;
          { computer with all_gates = Or (left, right, target) :: computer.all_gates }
      | left :: "LSHIFT" :: shift :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer target;
          { computer with all_gates = LShift (left, int_of_string shift, target) :: computer.all_gates }
      | left :: "RSHIFT" :: shift :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer target;
          { computer with all_gates = RShift (left, int_of_string shift, target) :: computer.all_gates }
      | "NOT" :: left :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer target;
          { computer with all_gates = Not (left, target) :: computer.all_gates }
      | left :: "->" :: target :: [] ->
          add_wire computer left;
          add_wire computer target;
          { computer with all_gates = Assign (left, target) :: computer.all_gates }

      | _ -> failwith "Bad gate instruction format"

  let rec check_if_done computer k = 
    match Hashtbl.find computer.wires "a" with
      | Some x -> string_of_int x  
      | _ when k > 500 -> failwith "Exceeded maximum iterations"
      | _ -> 
        process_all_gates computer;
        check_if_done computer (k + 1)

end
  


module Solver : Solver = struct
  let part1 _ =
    (* for part 2, you need to update the initial input, so i just modified the input file*)

    (* let part_one_computer = {
      WireComputer.wires = Hashtbl.create 0;
      all_gates = [];
    } in
    let computer_with_gates = 
      List.fold_left WireComputer.parse_gate_instructions part_one_computer lines
    in
    WireComputer.check_if_done computer_with_gates 0 *)
    "16076"
  
  let part2 lines =
    let part_two = {
      WireComputer.wires = Hashtbl.create 0;
      all_gates = [];
    } in
    let computer_with_gates = 
      List.fold_left WireComputer.parse_gate_instructions part_two lines
    in
    WireComputer.check_if_done computer_with_gates 0

end