let parse_map txt =
  let data = txt |> String.split_on_char '\n' |> Array.of_list in

  let is_wall (x, y) = 
    if y >= 0 && y < Array.length data && x >= 0 && x < String.length data.(0) then
      data.(y).[x] = '#'
    else
      false in
  
  let idx_to_xy i =
    let w = String.length data.(0) + 1 in
    (i mod w), i / w in

  let player = 'S' |> String.index txt |> idx_to_xy in
  let ending = 'E' |> String.index txt |> idx_to_xy in
  (player, (1, 0)), ending, is_wall

  let rotate_left (((x, y), (dx, dy)), score) = ((x, y), (-dy, dx)), (score + 1000)
  let rotate_right (((x, y), (dx, dy)), score) = ((x, y), (dy, -dx)), (score + 1000)
  let move_forward (((x, y), (dx, dy)), score) = ((x + dx, y+dy), (dx, dy)), (score + 1)

let compute_distance_matrix (init_player, is_wall) =
  let spots = Hashtbl.create 100000 in

  let rec check_state (state, curr_score) =
    let search () =
      Hashtbl.replace spots state curr_score;
      [move_forward; rotate_left; rotate_right]
      |> List.map (fun mv -> mv (state, curr_score))
      |> List.filter (fun ((pos, _), _) -> not (is_wall pos))
      |> List.iter check_state in

    match Hashtbl.find_opt spots state with
    | None -> search()
    | Some(best_score) when curr_score < best_score -> search()
    | _ -> () in

  check_state (init_player, 0);
  spots
let part1 txt =
  let (init_player, ending, is_wall) = parse_map txt in
  (init_player, is_wall)
  |> compute_distance_matrix
  |> Hashtbl.to_seq
  |> Seq.filter (fun ((pos, _), _) -> pos = ending)
  |> Seq.map (fun (_, score) -> score)
  |> Seq.fold_left min Int.max_int
  |> string_of_int

let check_for_best_path distance_matrix ending_state_and_scores =

  let inv_rotate_left (((x, y), (dx, dy)), score) = ((x, y), (dy, -dx)), (score - 1000) in
  let inv_rotate_right (((x, y), (dx, dy)), score) = ((x, y), (-dy, dx)), (score - 1000) in
  let inv_move_forward (((x, y), (dx, dy)), score) = ((x - dx, y-dy), (dx, dy)), (score - 1) in

  let best_paths = Hashtbl.create 1000 in
  let rec check_next_position state_and_score =
    [inv_rotate_left;inv_rotate_right;inv_move_forward]
    |> List.map (fun inv_mv -> inv_mv state_and_score)
    |> List.filter (fun (state, score) -> Hashtbl.find_opt distance_matrix state = Some(score))
    |> List.filter (fun (state, _) -> not(Hashtbl.mem best_paths state))
    |> List.iter (fun (state, score) ->
      Hashtbl.add best_paths state ();
      check_next_position (state, score)) in

  List.iter (fun (state, _) -> Hashtbl.add best_paths state ()) ending_state_and_scores;
  List.iter check_next_position ending_state_and_scores;
  Hashtbl.to_seq_keys best_paths
    

let compare (a1, a2) (b1, b2) =
  match compare a1 b1 with
  | 0 -> compare a2 b2
  | v -> v

let part2 txt = 
  let (init_player, ending, is_wall) = parse_map txt in
  let distance_matrix = compute_distance_matrix (init_player, is_wall) in

  let endings =
    distance_matrix
    |> Hashtbl.to_seq
    |> Seq.filter (fun ((pos, _), _) -> pos = ending) in
  let best_score =
    endings
    |> Seq.map(fun (_, score) -> score)
    |> Seq.fold_left min Int.max_int in


  endings
  |> Seq.filter (fun (_, score) -> score = best_score)
  |> List.of_seq
  |> check_for_best_path distance_matrix
  |> Seq.map (fun (pos, _) -> pos)
  |> List.of_seq
  |> List.sort_uniq compare
  (* |> List.iter (fun (x,y) -> print_int x; print_char ' '; print_int y; print_newline()); "" *)
  |> List.length
  |> string_of_int