module PointSet = Set.Make(struct 
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | v -> v
end);;

let load_data txt =
  let lines = txt |> String.split_on_char '\n' |> Array.of_list in
  let w, h = String.length lines.(0), Array.length lines in
  let top_height x y =
    let c = String.get lines.(y) x in
  (int_of_char c) - (int_of_char '0') in
  Array.init_matrix w h top_height

let part1 txt =
  let map = load_data txt in
  let get (x, y) = if x >=0 && x < Array.length map && y >= 0 && y < Array.length map.(0) then map.(x).(y) else -1 in
  let compute_score x y =
    if get (x, y) != 0 then 0 else
    let rec helper curr_height points =
      if curr_height == 9 then PointSet.cardinal points else
      let test_neighbors (x, y) =
        [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
        |> List.to_seq
        |> Seq.filter (fun pt -> get pt == curr_height+1) in 
      points
      |> PointSet.to_seq
      |> Seq.map test_neighbors
      |> Seq.concat
      |> PointSet.of_seq
      |> helper (curr_height+1) in
    helper 0 (PointSet.add (x, y) PointSet.empty) in

    let net_score = ref 0 in
    for x = 0 to Array.length map do
      for y = 0 to Array.length map do
        net_score := !net_score + compute_score x y
      done
    done;
    string_of_int (!net_score)

let part2 txt =
  let map = load_data txt in
  let get (x, y) = if x >=0 && x < Array.length map && y >= 0 && y < Array.length map.(0) then map.(x).(y) else -1 in
  let compute_score x y =
    if get (x, y) != 0 then 0 else
    let rec helper curr_height point =
      if curr_height == 9 then 1 else
      let test_neighbors (x, y) =
        [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
        |> List.to_seq
        |> Seq.filter (fun pt -> get pt == curr_height+1) in 
      point
      |> test_neighbors
      |> Seq.map (helper (curr_height+1))
      |> Seq.fold_left (+) 0 in
    helper 0 (x,y) in

    let net_score = ref 0 in
    for x = 0 to Array.length map do
      for y = 0 to Array.length map do
        net_score := !net_score + compute_score x y
      done
    done;
    string_of_int (!net_score)