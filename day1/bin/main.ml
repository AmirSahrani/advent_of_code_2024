let input = "data/input"

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

let split data = List.map (fun x -> Str.split (Str.regexp "[ ]+") x) data

let unzip data =
  List.fold_right
    (fun x (l1, l2) ->
      match x with
      | [ a; b ] -> (a :: l1, b :: l2)
      | _ -> failwith "Invalid data format")
    data ([], [])

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let prod_count left right =
  List.map
    (fun l ->
      let count =
        List.fold_right (fun r acc -> if r = l then acc + 1 else acc) right 0
      in
      l * count)
    left

let sort_string lst = List.sort compare (List.map int_of_string lst)
let diff_lists l1 l2 = List.map2 (fun l r -> abs (l - r)) l1 l2

let part_1 left right =
  let diff = diff_lists left right in
  List.fold_left ( + ) 0 diff

let () =
  let left, right = unzip (split (read_lines input)) in
  let left = sort_string left in
  let right = sort_string right in
  let sum = time part_1 left right in
  Printf.printf "Sum: %d\n" sum;

  let prod = time prod_count left right in
  Printf.printf "Prod: %d\n" (List.fold_left ( + ) 0 prod)
