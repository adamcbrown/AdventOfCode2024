type problem = {
  da: int * int;
  db: int * int;
  target: int * int;
} [@@deriving show]

let split_on_substring substr str =
  let list = ref [] in
  let last_idx = ref 0 in
  let split_size = String.length substr in
  for i = 0 to String.length str-1-split_size do
    if String.sub str i split_size = substr then (
      list := String.sub str !last_idx (i - !last_idx) :: !list;
      last_idx := i + split_size;
    )
  done;
  list := String.sub str !last_idx (String.length str - !last_idx) :: !list;
  List.rev !list

let load txt =
  let parse_problem txt =
    let lines = String.split_on_char '\n' txt |> Array.of_list in
    let parse line c =
      let startX = String.index line c + 1 in
      let endX = String.index line ',' in
      let startY = endX + 4 in
      let endY = String.length line in
      let dx = String.sub line startX (endX - startX)
      and dy = String.sub line startY (endY - startY) in
      (* print_string dx; print_string dy; print_newline(); *)
      (int_of_string dx, int_of_string dy) in
    let da = parse lines.(0) '+'
    and db = parse lines.(1) '+'
    and target = parse lines.(2) '=' in
    {da; db; target} in
  txt
  |> split_on_substring "\n\n"
  |> List.map parse_problem

  (* ax*i + bx*j = tx *)
  (* ay*i + by*j = ty *)
  (*  *)
let part1 txt =
  let solve_problem prob =
    let (a, c), (b, d), (tx, ty) = prob.da, prob.db, prob.target in
    let detem = (d * a - b * c) in
    let a_presses_num = (d * tx - b * ty) 
    and b_presses_num = (-c * tx + a * ty) in
    if a_presses_num mod detem == 0 && b_presses_num mod detem == 0 then
      Some(a_presses_num/detem * 3 + b_presses_num/detem) 
    else
      None in
  txt
  |> load
  |> List.filter_map solve_problem
  |> List.fold_left (+) 0
  |> string_of_int

let part2 txt =
  let update_prob prob = {prob with target = let (x, y) = prob.target in (x+10000000000000, y+10000000000000)} in
  let solve_problem prob =
    let (a, c), (b, d), (tx, ty) = prob.da, prob.db, prob.target in
    let detem = (d * a - b * c) in
    let a_presses_num = (d * tx - b * ty) 
    and b_presses_num = (-c * tx + a * ty) in
    if a_presses_num mod detem == 0 && b_presses_num mod detem == 0 then
      Some(a_presses_num/detem * 3 + b_presses_num/detem) 
    else
      None in
  txt
  |> load
  |> List.map update_prob
  |> List.filter_map solve_problem
  |> List.fold_left (+) 0
  |> string_of_int