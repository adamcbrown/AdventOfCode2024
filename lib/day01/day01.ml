let (|>) v f = f v

let process_input txt =
  let process_line line =
    let first_idx = String.index line ' '
    and second_idx = String.rindex line ' ' + 1
    and len = String.length line in
    let first = String.sub line 0 first_idx
    and second = String.sub line second_idx (len - second_idx) in

    (int_of_string first, int_of_string second)
  in

  let lines = String.split_on_char '\n' txt in
  let parts = List.map process_line lines in

  let rec acc a b rest =
    match rest with
    | [] -> (a, b)
    | ((x,y)::rest) -> acc (x::a) (y::b) rest
  in
  acc [] [] parts

  let part1 txt =
    let (left, right) = process_input txt in
    let sorted_left = List.sort compare left
    and sorted_right = List.sort compare right
    in
    (* List.iter (fun i -> print_int i; print_newline()) sorted_left; *)
    List.map2 (-) sorted_right sorted_left
    |> List.map abs
    |> List.fold_left (+) 0
    |> string_of_int

  let compute_similarity_score list =
    let tbl = Hashtbl.create (List.length list) in
    let incr i = match Hashtbl.find_opt tbl i with
      | None -> Hashtbl.add tbl i 1
      | Some(count) -> Hashtbl.replace tbl i (count + 1)
    in
    List.iter incr list;
    tbl

  let part2 txt =
    let (left, right) = process_input txt in
    let right_similarities = compute_similarity_score right in
    let similarity i = match Hashtbl.find_opt right_similarities i with
      | None -> 0
      | Some(count) -> count
    in
    List.map (fun i -> i * similarity i) left
    |> List.fold_left (+) 0
    |> string_of_int