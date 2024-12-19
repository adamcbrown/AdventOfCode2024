let parse_map w h txt num_points =

  let points =
    txt
    |> String.split_on_char '\n'
    |> List.map (String.split_on_char ',')
    |> List.map (List.map int_of_string)
    |> List.map (fun l -> match l with | (x::y::[]) -> (x, y) | _ -> raise (invalid_arg "bad"))
    |> List.to_seq
    |> Seq.take num_points
    |> (fun xy -> Seq.zip xy (Seq.repeat ()))
    |> Hashtbl.of_seq in

  let is_wall (x,y) =
    if x >= 0 && x < w && y >= 0 && y < h then
      Option.is_some (Hashtbl.find_opt points (x,y))
    else
      true
    in
  
  is_wall

let (+.) (x1, y1) (x2, y2) = (x1 + x2, y1+y2)

let compute_distance_matrix (init_player, is_wall) =
  let spots = Hashtbl.create 100000 in

  let rec check_state (state, curr_score) =
    let search () =
      Hashtbl.replace spots state curr_score;
      [(0, 1); (0, -1); (1, 0); (-1, 0)]
      |> List.map (fun dv -> (state +. dv, curr_score+1))
      |> List.filter (fun (pos, _) -> not (is_wall pos))
      |> List.iter check_state in

    match Hashtbl.find_opt spots state with
    | None -> search()
    | Some(best_score) when curr_score < best_score -> search()
    | _ -> () in

  check_state (init_player, 0);
  spots
let part1 txt =
  let w, h = 71, 71 in
  let is_wall = parse_map w h txt Int.max_int in
  ((0,0), is_wall)
  |> compute_distance_matrix
  |> (Fun.flip Hashtbl.find) (w-1, h-1)
  |> string_of_int

let bin_search fn min max =
  let rec helper min max =
    print_int min; print_char ' '; print_int max; print_newline();
    if max - min = 1 then max else 
    let i = (max + min) / 2 in
    if fn i then helper min i else helper i max in
  helper min max

let part2 txt =
  let w, h = 71, 71 in
  let not_possible i =
    let is_wall = parse_map w h txt i in
    ((0,0), is_wall)
    |> compute_distance_matrix
    |> (Fun.flip Hashtbl.find_opt) (w-1, h-1)
    |> Option.is_none in
  bin_search not_possible 0 3450
  |> string_of_int
