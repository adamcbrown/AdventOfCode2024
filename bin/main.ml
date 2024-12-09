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
    | v -> raise (Invalid_argument v)
  in
  print_string (fn txt); print_newline()
;;