open Stdlib

let () =
  let txt = In_channel.input_all In_channel.stdin in
  let fn =
    match Sys.argv.(1) with
    | "1.1" -> Day01.part1
    | "1.2" -> Day01.part2
    | "2.1" -> Day02.part1
    | "2.2" -> Day02.part2
    | "3.1" -> Day03.part1
    | "3.2" -> Day03.part2
    | "4.1" -> Day04.part1
    | "4.2" -> Day04.part2
    | "5.1" -> Day05.part1
    | "5.2" -> Day05.part2
    | "6.1" -> Day06.part1
    | "6.2" -> Day06.part2
    | "7.1" -> Day07.part1
    | "7.2" -> Day07.part2
    | "8.1" -> Day08.part1
    | "8.2" -> Day08.part2
    | "9.1" -> Day09.part1
    | "9.2" -> Day09.part2
    | "10.1" -> Day10.part1
    | "10.2" -> Day10.part2
    | "11.1" -> Day11.part1
    | "11.2" -> Day11.part2
    | "12.1" -> Day12.part1
    | "12.2" -> Day12.part2
    | "13.1" -> Day13.part1
    | "13.2" -> Day13.part2
    | "14.1" -> Day14.part1
    | "14.2" -> Day14.part2
    | "15.1" -> Day15.part1
    | "15.2" -> Day15.part2
    | "16.1" -> Day16.part1
    | "16.2" -> Day16.part2
    | "17.1" -> Day17.part1
    | "17.2" -> Day17.part2
    | "18.1" -> Day18.part1
    | "18.2" -> Day18.part2
    | v -> raise (Invalid_argument v)
  in
  print_string (fn txt); print_newline()
;;