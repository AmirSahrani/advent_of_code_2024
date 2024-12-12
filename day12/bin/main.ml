open In_channel
open Printf
(* open Set *)

[@@@ocaml.warning "-32"]

let readdata ic = input_lines ic


let rec split str  =
  let len = String.length str in
  if  len = 0 then [] else
  str.[0] :: split (String.sub str 1 len)

module Set.Make(int*int);;

let part_1 matrix =
  let already_include =


let () =
  let data = readdata (open_in "data/test") in
  let sdata = List.map split data in
  let arrays = List.map Array.of_list sdata in
  let mat = Array.of_list arrays in
  let perimiter = part_1 mat
