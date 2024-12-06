let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in
  loop ()

module TupleSet = Set.Make (struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with 0 -> compare a2 b2 | c -> c
end)

(* let print_set set =
  TupleSet.iter (fun (x, y) -> Printf.printf "(%d| %d)\n" x y) set *)

let next_position lst_str indx pos (vertical, horizontal) =
  (List.nth lst_str (indx + vertical)).[pos + horizontal]

let turn_90_degrees (horizontal, vertical) =
  match (horizontal, vertical) with
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> failwith "invalid input"

let patrol_guard lst_str start_indx start_pos =
  let starting_direction = (-1, 0) in
  let rec move indx pos (x, y) history =
    try
      (* Check grid boundaries *)
      if indx = 0 || indx = List.length lst_str then failwith "Guard left grid";
      let row = List.nth lst_str indx in
      if pos = 0 || pos = String.length row then failwith "Guard left grid";

      (* Get the next tile and update history *)
      let next_tile = next_position lst_str indx pos (x, y) in
      let history = TupleSet.add (indx, pos) history in

      (* Decide the next move based on tile type *)
      match next_tile with
      | '#' -> move indx pos (turn_90_degrees (x, y)) history
      | _ -> move (indx + x) (pos + y) (x, y) history
    with Failure _ -> history
  in
  move start_indx start_pos starting_direction TupleSet.empty

let find_start lststr =
  let indx = List.find_index (fun x -> String.contains x '^') lststr in
  match indx with
  | None -> failwith "not found"
  | Some x -> (x, String.index (List.nth lststr x) '^')

let () =
  let data = read_lines "data/input" in
  let x, y = find_start data in
  Printf.printf "%d, %d\n" x y;
  let history = patrol_guard data x y in
  (* print_set history; *)
  print_int (TupleSet.cardinal history);
  print_string "\n"
