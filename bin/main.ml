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
    | v -> raise (Invalid_argument v)
  in
  print_string (fn txt); print_newline()
;;