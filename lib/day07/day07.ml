let load_data txt =
  let load_row line =
    let idx = String.index line ':' in
    let target = String.sub line 0 idx in
    let nums =
      String.sub line (idx+2) (String.length line - idx - 2)
      |> String.split_on_char ' '
      |> List.map int_of_string in
    (int_of_string target, nums) in
  txt
  |> String.split_on_char '\n'
  |> List.map load_row

let part1 txt =
  let is_valid (target, instrs) =
    let rec helper target instrs = match target, instrs with
    | x, [y] when x == y -> true
    | _, [] -> false
    | t, (x::xs) -> 
      (if t mod x = 0 then helper (t/x) xs else false) ||
      (if t >= x then helper (t-x) xs else false)
    in
    helper target (List.rev instrs) in
  txt
  |> load_data 
  |> List.filter is_valid
  |> List.map (fun (target, _) -> target)
  |> List.fold_left (+) 0
  |> string_of_int

  let part2 txt =
    let is_valid (target, instrs) =
      let rec helper target instrs = match target, instrs with
      | x, [y] when x == y -> true
      | _, [] -> false
      | t, (x::xs) -> 
        (if t mod x = 0 then helper (t/x) xs else false) ||
        (if t >= x then helper (t-x) xs else false) ||
        (if x != t && String.ends_with ~suffix:(string_of_int x) (string_of_int t) then
          let t_str, x_str = string_of_int t, string_of_int x in
          let subbed = int_of_string (String.sub t_str 0 (String.length t_str - String.length x_str)) in
          helper subbed xs
        else false)
      in
      helper target (List.rev instrs) in
    txt
    |> load_data 
    |> List.filter is_valid
    |> List.map (fun (target, _) -> target)
    |> List.fold_left (+) 0
    |> string_of_int