open In_channel

[@@@ocaml.warning "-32"]

let read ic = input_lines ic

let () =
  let part1, part2 = read (open_in "data/input_day1") |> Aoc.Day1.solve in
  Printf.printf "Part 1: %d\n Part 2 %d" part1 part2
;;
