let letter c = int_of_char c - int_of_char 'a'
let num i = char_of_int (int_of_char 'a' + i)

let parse_data txt =
  let to_index comp = (letter comp.[0]) * 26 + letter comp.[1] in
  let parse_row row =
    match String.split_on_char '-' row |> List.map to_index with
    | x::y::[] -> (x, y)
    | _ -> assert false in

  txt
  |> String.split_on_char '\n'
  |> List.map parse_row

let to_comp i = [num (i/26); num (i mod 26)] |> List.to_seq |> String.of_seq


let build_adjacency_table pairs =
  let data = Array.make_matrix (26*26) (26*26) false in

  let rec helper pairs = match pairs with
  | (x,y)::rest ->
    data.(x).(y) <- true;
    data.(y).(x) <- true;
    helper rest
  | [] -> () in

  helper pairs;
  data


let part1 txt =
  let clique_has_t (a, b, c) =
    let starts_with_t i = (i / 26) = int_of_char 't' - int_of_char 'a' in
    starts_with_t a || starts_with_t b || starts_with_t c in

  let find_cliques data =
    let cliques = ref [] in
    let len = 26*26-1 in
    for i = 0 to len do
      for j = i+1 to len do
        if data.(i).(j) then
          for k = j+1 to len do
            if data.(i).(k) && data.(j).(k) then
              cliques := (i, j, k) :: !cliques
          done
      done
    done;
    !cliques in

  txt
  |> parse_data
  |> build_adjacency_table
  |> find_cliques
  |> List.filter clique_has_t
  |> List.length
  |> string_of_int

  let part2 txt =
    let len = 26*26 in
    let find_all_cliques data =
      let valid nodes test = List.for_all (fun i -> data.(i).(test)) nodes in

      let all_cliques = ref [] in
      let rec build_clique so_far next =
        let so_far = next :: so_far in
        let next = Seq.ints (next+1)
        |> Seq.take (len - next - 1)
        |> Seq.filter (valid so_far) in
        if Seq.is_empty next then 
          all_cliques := (so_far) :: !all_cliques
        else
          Seq.iter (build_clique so_far) next in
      
      Seq.ints 0
      |> Seq.take len
      |> Seq.iter (build_clique []);
      !all_cliques in
  
    txt
    |> parse_data
    |> build_adjacency_table
    |> find_all_cliques
    |> List.map (fun clique -> (clique, List.length clique))
    |> List.fold_left (fun (c1, l1) (c2, l2) -> if l1 > l2 then (c1, l1) else (c2, l2)) ([], -1)
    |> fst
    |> List.rev
    |> List.map to_comp
    |> String.concat ","