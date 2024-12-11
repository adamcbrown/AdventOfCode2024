module IntPairSet = Set.Make(struct 
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | v -> v
end);;

let load_map txt =
  let map = Hashtbl.create 100 in
  let iter_row y row =
    let iter_col x char = match char with
    | l when l != '.'->
      let curr_list = match Hashtbl.find_opt map l with
      | Some(list) -> list
      | None -> [] in
      Hashtbl.replace map l ((x,y)::curr_list)
    | _ -> () in
    String.iteri iter_col row in
  let rows = String.split_on_char '\n' txt in
  List.iteri iter_row rows;
  map, String.length (List.hd rows), (List.length rows)

let rec pairs xs =
  match  xs with
  | x::xs -> Seq.append (Seq.map (fun y -> (x, y)) (List.to_seq xs)) (pairs xs)
  | [] -> Seq.empty


let compute_antinode_positions antinode_fn positions =
  positions
  |> pairs
  |> Seq.map antinode_fn
  |> Seq.concat 

  let part1 txt =
    let pt_1_antinodes ((x1, y1), (x2, y2)) =
      let (dx, dy) = (x2-x1, y2-y1) in
      List.to_seq [(x1-dx, y1-dy);(x2+dx, y2+dy)] in
    let map, w, h = load_map txt in
    let in_map (x, y) = x >= 0 && x < w && y >= 0 && y < h in
    map
    |> Hashtbl.to_seq_values
    |> Seq.map (compute_antinode_positions pt_1_antinodes)
    |> Seq.concat
    |> Seq.filter in_map
    |> IntPairSet.of_seq
    |> IntPairSet.cardinal
    |> string_of_int

  let part2 txt =
    let map, w, h = load_map txt in
    let in_map (x, y) = x >= 0 && x < w && y >= 0 && y < h in
    let pt_2_antinodes ((x1, y1), (x2, y2)) =
      let (dx, dy) = (x2-x1, y2-y1) in
      let rec lower_seq (x, y) =
        if not (in_map (x, y)) then Seq.empty
        else Seq.cons (x, y) (lower_seq (x-dx, y-dy)) in
      let rec upper_seq (x, y) =
        if not (in_map (x, y)) then Seq.empty
        else Seq.cons (x, y) (upper_seq (x+dx, y+dy)) in
      Seq.append (lower_seq (x1, y1)) (upper_seq (x2, y2)) in
    map
    |> Hashtbl.to_seq_values
    |> Seq.map (compute_antinode_positions pt_2_antinodes)
    |> Seq.concat
    |> IntPairSet.of_seq
    |> IntPairSet.cardinal
    |> string_of_int