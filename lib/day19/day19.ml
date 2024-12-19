let parse_data txt = 
  let lines = String.split_on_char '\n' txt in
  let patterns =
    lines
    |> List.hd
    |> String.split_on_char ','
    |> List.map String.trim in
  (patterns, lines |> List.tl |> List.tl)

let from str i = String.sub str i (String.length str - i)

let count_methods patterns design =
  let memoized = Hashtbl.create (String.length design) in
  let rec helper rest_design =
    if rest_design = "" then 1 else
    match Hashtbl.find_opt memoized rest_design with
    | Some(num) -> num
    | None ->
      let num_methods = 
        patterns
        |> List.filter (fun pattern -> String.starts_with rest_design ~prefix:pattern)
        |> List.map (fun pattern -> from rest_design (String.length pattern))
        |> List.map helper
        |> List.fold_left (+) 0 in
      Hashtbl.add memoized rest_design num_methods;
      num_methods in
  helper design

  let part1 txt =
  let patterns, designs = parse_data txt in
  designs
  |> List.map (count_methods patterns)
  |> List.filter (fun num -> num > 0)
  |> List.length
  |> string_of_int

let part2 txt =
  let patterns, designs = parse_data txt in
  designs
  |> List.map (count_methods patterns)
  |> List.fold_left (+) 0
  |> string_of_int