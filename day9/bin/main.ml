open In_channel

let readdata ic =
  let out = input_line ic in
  match out with None -> "" | Some x -> x

let rec str_to_list str =
  let length = String.length str in
  match str with
  | "" -> []
  | x ->
      (int_of_char x.[0] - int_of_char '0')
      :: str_to_list (String.sub str 1 (length - 1))

let rec repeat value count =
  if count > 0 then value :: repeat value (count - 1) else []

let rec gen_disk lst contains_data data_index =
  match (lst, contains_data) with
  | [], _ -> []
  | hd :: tl, false -> repeat None hd @ gen_disk tl true data_index
  | hd :: tl, true ->
      repeat (Some data_index) hd @ gen_disk tl false (data_index + 1)

let rec remove_none lst =
  match lst with
  | [] -> []
  | None :: tl -> remove_none tl
  | Some x :: tl -> x :: remove_none tl

let drop_lst n lst =
  let length = List.length lst in
  let out = List.filteri (fun i _ -> i < length - n) lst in
  out

let fill_gaps lst =
  (* let lst_ref = ref lst in *)
  let possible = ref (remove_none (List.rev lst)) in
  List.fold_left_map
    (fun acc x ->
      match x with
      | None -> (
          try
            let hd = List.hd !possible in
            possible := List.tl !possible;
            (acc + 1, hd)
          with Failure _ -> (acc, 0))
      | Some x -> (acc, x))
    0 lst

let part_1 data =
  let lst_data = str_to_list data in
  let disk = gen_disk lst_data true 0 in
  let n, filled = fill_gaps disk in
  let dropped = drop_lst n filled in
  let check_sum = List.mapi (fun i x -> i * x) dropped in
  let total = List.fold_right (fun x acc -> acc + x) check_sum 0 in
  Printf.printf "Part 1: %d\n" total

let () =
  let ic = open_in "data/input" in
  let data = readdata ic in
  part_1 data
