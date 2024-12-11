open In_channel
open Printf

let read_data ic = input_lines ic

let rec split str =
  let str_length = String.length str in
  if str_length = 0 then []
  else str.[0] :: split (String.sub str 1 (str_length - 1))

(* let print_mat mat =
  Array.iter
    (fun row ->
      Array.iter (fun entry -> printf "%c" entry) row;
      print_endline "")
    mat *)

let get_neighbors mat i j =
  let directions = [| (0, 1); (0, -1); (1, 0); (-1, 0) |] in
  Array.map
    (fun (di, dj) ->
      let new_i = i + di in
      let new_j = j + dj in
      if
        new_i > Array.length mat - 1
        || new_i < 0 || new_j < 0
        || new_j > Array.length mat - 1
      then ('x', 0, 0)
      else (mat.(new_i).(new_j), new_i, new_j))
    directions

let next_char c = Char.chr (Char.code c + 1)

let part_1 mat =
  let counter = ref 0 in
  let rec aux i j tgt hist =
    let neighbors = get_neighbors mat i j in
    Array.iter
      (fun (neighbor, new_i, new_j) ->
        if neighbor = tgt then
          if neighbor = '9' then
            if not (List.mem (new_i, new_j) !hist) then (
              incr counter;
              hist := (new_i, new_j) :: !hist)
            else ()
          else aux new_i new_j (next_char neighbor) hist
        else ())
      neighbors
  in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j entry ->
          if entry = '0' then
            let hist = ref [] in
            aux i j '1' hist
          else ())
        row)
    mat;
  !counter

let part_2 mat =
  let counter = ref 0 in
  let rec aux i j tgt =
    let neighbors = get_neighbors mat i j in
    Array.iter
      (fun (neighbor, new_i, new_j) ->
        if neighbor = tgt then
          if neighbor = '9' then incr counter
          else aux new_i new_j (next_char neighbor)
        else ())
      neighbors
  in
  Array.iteri
    (fun i row ->
      Array.iteri (fun j entry -> if entry = '0' then aux i j '1' else ()) row)
    mat;
  !counter

let () =
  let data = read_data (open_in "data/test") in
  let split_data = List.map split data in
  let list_of_arrays = List.map Array.of_list split_data in
  let mat = Array.of_list list_of_arrays in
  let part_1_reult = part_1 mat in
  let part_2_reult = part_2 mat in
  printf "Part 1: %d\n" part_1_reult;
  printf "Part 2: %d\n" part_2_reult
