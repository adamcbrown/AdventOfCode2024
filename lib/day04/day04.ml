let load_map txt = txt |> String.split_on_char '\n' |> Array.of_list

let get map x y =
  if y >= 0 && y < Array.length map && x >= 0 && x < String.length map.(0) then
    map.(y).[x]
  else
    '.'


let part1 txt =
  let dirs = [(1,0);(1,1);(0,1);(-1,1);(-1,0);(-1,-1);(0,-1);(1,-1)] in
  let map = load_map txt in
  let char = get map in
  let count_xmas i =
    let w = String.length map.(0) in
    let x, y = i mod w, i / w in
    let count_xmas_dir (dx, dy) =
      char x y == 'X' &&
      char (x+dx) (y+dy) == 'M' &&
      char (x+2*dx) (y+2*dy) == 'A' &&
      char (x+3*dx) (y+3*dy) == 'S' in
    dirs |> List.filter count_xmas_dir |> List.length
    in
  Seq.init (Array.length map * (String.length map.(0))) count_xmas
  |> Seq.fold_left (+) 0
  |> string_of_int
            
  let part2 txt = 
    let map = load_map txt in
    let char = get map in
    let is_x_mas i =
      let w = String.length map.(0) in
      let x, y = i mod w, i / w in
      char x y == 'A' &&
      match char (x-1) (y-1), char (x+1) (y+1), char (x+1) (y-1), char (x-1) (y+1) with
      | 'M', 'S', 'M', 'S' -> true
      | 'S', 'M', 'M', 'S' -> true
      | 'S', 'M', 'S', 'M' -> true
      | 'M', 'S', 'S', 'M' -> true
      | _ -> false
      in
    Seq.init (Array.length map * (String.length map.(0))) (fun i -> i, is_x_mas i)
    |> Seq.filter (fun (i, x) -> if x then print_int i; print_newline(); x)
    |> Seq.length
    |> string_of_int