open Day15

type tile =
  | Wall
  | Tile
  | Target
  | Source

let parse_tile t =
  match t with
  | '#' -> Wall
  | '.' -> Tile
  | 'E' -> Target
  | 'S' -> Source
  | _ -> failwith "Invalid tile"
;;

let parse data =
  Array.of_list (List.map (fun x -> split x |> List.map parse_tile |> Array.of_list) data)
;;

let dir_of_pi p = int_of_float @@ sin p, int_of_float @@ -.cos p

let weight_of_tile tile pentaly =
  match tile with
  | Wall -> infinity
  | _ -> 1.0 +. pentaly
;;

let weight_matrix_like mat =
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  let mat_out = Array.init_matrix rows cols (fun _ _ -> infinity) in
  mat_out
;;

let locate t grid =
  let i = Array.find_index (fun x -> Array.exists (fun y -> y = t) x) grid in
  let idx =
    match i with
    | Some idx -> idx
    | _ -> failwith "No found"
  in
  let j = Array.find_index (fun x -> x = t) grid.(idx) in
  match j with
  | Some jdx -> idx, jdx
  | _ -> failwith "Not found"
;;

module PrioritySet = Set.Make (struct
    type t = int * int * int

    let compare (_, _, a) (_, _, b) = if a > b then 1 else if a > b then -1 else 0
  end)


let get_neighbors mat (i,j) dir = 
  let turns = [-.Float.pi/.2.0;  0.0; Float.pi/.2.0] in
  let penalties = [1000.0, 0.0 , 1000.0] in
  let turns = List.map (fun x -> dir +. x |> dir_of_pi) turns in
  List.map2 (fun (di, dj) penalty -> 
    try
  let tile =  mat.(i+di).(j+dj)
    match tile with 
      | Wall -> i+di,j+dj, infinity
      | _ -> i+di,j+dj, (1.0 +. penalty)
    with Invalid_argument _ ->
      i+di,j+dj, infinity
  ) turns penalties
      

let cheapest_path mat =
  let weight_mat = weight_matrix_like mat in
  let visited = PrioritySet.empty in
  let si, sj = locate Source mat in
  let visited = PrioritySet.add (si, sj, 0), visited in
  let target = locate Target mat in
  let rec aux dir (i, j) =
    let neighbors = get_neighbors mat (i, j) dir in
    List.iter (fun (ni, nj, c) ->
      if weight_mat.(ni).(nj) > weight_mat.(i).(j) +. c
      then weight_mat.(ni).(nj) <- weight_mat.(i).(j) +. c) neighbors in
    let next = PrioritySet.min_elt visited in
    aux  
  in
  aux 0 (si, sj)
;;

let part1 _ = 0
let part2 _ = 0
let solve data = part1 data, part2 0
