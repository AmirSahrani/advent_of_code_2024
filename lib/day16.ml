open Day6

type tile =
  | Wall
  | Tile

let dir_of_pi p = int_of_float @@ sin p, int_of_float @@ -.cos p

let weight_of_tile tile pentaly =
  match tile with
  | Wall -> 1000_000_000
  | Tile -> 1 + pentaly
;;

let weight_matrix_like mat source =
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  let mat_out =
    Array.init_matrix rows cols (fun x y -> if (x, y) = source then 0 else 1000_000_000)
  in
  mat_out
;;

let assign_costs mat =
  let visited = Day6.TupleSet.empty in
  let rec aux dir pentaly loc =
    


let part1 _ = 0
let part2 _ = 0
let solve data = part1 data, 0
