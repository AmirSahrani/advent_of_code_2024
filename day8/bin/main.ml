open In_channel
open Printf

[@@@ocaml.warning "-32-26"]

let readfile ic = input_lines ic

let rec print_list lst =
  match lst with
  | [] -> ()
  | hd :: tl ->
      print_char hd;
      print_list tl

let to_matrix lst height width =
  Array.init_matrix height width (fun x y -> (List.nth lst x).[y])

let get_frequencies_and_pos matrix =
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  let map = Hashtbl.create height in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let current = matrix.(i).(j) in
      if current <> '.' then Hashtbl.add map current (current, (i, j))
    done
  done;
  map

let rec project (fromx, fromy) (throughx, throughy) max_x max_y mult =
  if fromx = throughx && fromy = throughy then []
  else
    let x_dist = throughx - fromx in
    let y_dist = throughy - fromy in
    let next_x = throughx + (x_dist * mult) in
    let next_y = throughy + (y_dist * mult) in
    if next_x >= 0 && next_y >= 0 && next_x < max_x && next_y < max_y then
      (next_x, next_y)
      :: project (fromx, fromy) (throughx, throughy) max_x max_y (mult + 1)
    else []

let rec all_pairs lst =
  match lst with
  | [] -> []
  | x :: tl ->
      List.map (fun y -> (x, y)) lst
      @ List.map (fun y -> (y, x)) lst
      @ all_pairs tl

let check_nodes tbl max_x max_y =
  List.flatten
    (Hashtbl.fold
       (fun frequency _ acc ->
         let points = Hashtbl.find_all tbl frequency in
         let pairs = all_pairs points in
         let results =
           List.map
             (fun ((_, pos1), (_, pos2)) -> project pos1 pos2 max_x max_y 0)
             pairs
         in
         results @ acc)
       tbl [])

let () =
  let ic = open_in "data/input" in
  let data = readfile ic in
  let array =
    to_matrix data (List.length data) (String.length (List.nth data 0))
  in
  let max_x = Array.length array in
  let max_y = Array.length array.(0) in
  let hash = get_frequencies_and_pos array in
  let out = check_nodes hash max_x max_y in
  let _ = List.iter (fun (i, j) -> Array.set (Array.get array i) j '#') out in
  let _ =
    Array.iter
      (fun x ->
        print_list (Array.to_list x);
        print_endline "")
      array
  in
  let sum_part_1 =
    Array.fold_right
      (fun arr acc ->
        Array.fold_right (fun y acc -> if y = '#' then acc + 1 else acc) arr acc)
      array 0
  in
  printf "Part 1: %d\n" sum_part_1
