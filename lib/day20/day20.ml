let parse_map txt =
  let data = txt |> String.split_on_char '\n' |> Array.of_list in

  let is_wall (x, y) = 
    if y >= 0 && y < Array.length data && x >= 0 && x < String.length data.(0) then
      data.(y).[x] = '#'
    else
      true in
  
  let w, h = String.length data.(0), Array.length data in
  let idx_to_xy i =
    (i mod (w+1)), i / (w+1) in

  let player = 'S' |> String.index txt |> idx_to_xy in
  let ending = 'E' |> String.index txt |> idx_to_xy in
  player, ending, (w, h), is_wall



let (+.) (x1, y1) (x2, y2) = (x1 + x2, y1+y2)

let compute_distance_matrix (init_player, is_wall) =
  let spots = Hashtbl.create 0 in

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
    let init_player, _, _, is_wall = parse_map txt in
    
    let distance_matrix = compute_distance_matrix (init_player, is_wall) in
      
    let points =
      Hashtbl.to_seq distance_matrix
      |> List.of_seq
      |> List.sort (fun a b -> compare (snd a) (snd b)) in
  
    let distance (x1, y1) (x2, y2) = abs(x1-x2) + abs(y1-y2) in
    
    let score (s, e) = (snd e - snd s) - (distance (fst e) (fst s)) in
  
    let search s =
      points
      |> List.filter (fun e -> distance (fst s) (fst e) <= 2)
      |> List.map (fun e -> (s, e)) in
  
    points
    |> List.map search
    |> List.concat
    |> List.map score
    |> List.filter (fun s -> s >= 100)
    |> List.length
    |> string_of_int

let part2 txt =
  let init_player, _, _, is_wall = parse_map txt in
  
  let distance_matrix = compute_distance_matrix (init_player, is_wall) in
    
  let points =
    Hashtbl.to_seq distance_matrix
    |> List.of_seq
    |> List.sort (fun a b -> compare (snd a) (snd b)) in

  let distance (x1, y1) (x2, y2) = abs(x1-x2) + abs(y1-y2) in
  
  let score (s, e) = (snd e - snd s) - (distance (fst e) (fst s)) in

  let search s =
    points
    |> List.filter (fun e -> distance (fst s) (fst e) <= 20)
    |> List.map (fun e -> (s, e)) in

  points
  |> List.map search
  |> List.concat
  |> List.map score
  |> List.sort compare
  |> List.filter (fun s -> s >= 100)
  |> List.length
  |> string_of_int