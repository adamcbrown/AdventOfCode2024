let numpad_coord c = match c with
| '7' -> (0, 0)
| '8' -> (1, 0) 
| '9' -> (2, 0) 
| '4' -> (0, 1) 
| '5' -> (1, 1) 
| '6' -> (2, 1) 
| '1' -> (0, 2) 
| '2' -> (1, 2) 
| '3' -> (2, 2) 
| '0' -> (1, 3) 
| 'A' -> (2, 3)
| _ -> assert false

let arrowpad_coord c = match c with
| '^' -> (1, 0)
| 'A' -> (2, 0)
| '<' -> (0, 1)
| 'v' -> (1, 1)
| '>' -> (2, 1)
| _ -> assert false

let n_chars c n = c |> Seq.repeat |> Seq.take n

let cached_arrowpad_move = Hashtbl.create 10000

let rec best_arrowpad_seq seq depth =
  let best_arrowpad_move s e =
    let move = (s, e, depth) in
    match Hashtbl.find_opt cached_arrowpad_move move with
    | Some(l) -> l
    | None -> 
    let seq = String.to_seq (match s, e with
    | '>', '^' ->  "<^A"
    | '>', 'v' ->  "<A"
    | '>', 'A' ->  "^A"
    | 'v', '<' ->  "<A"
    | 'v', '>' ->  ">A"
    | 'v', 'A' ->  "^>A"
    | '^', '<' ->  "v<A"
    | '^', '>' ->  "v>A"
    | '^', 'A' ->  ">A"
    | '<', 'v' ->  ">A"
    | '<', '^' ->  ">^A"
    | '<', 'A' ->  ">>^A"
    | 'A', '^' ->  "<A"
    | 'A', 'v' ->  "<vA"
    | 'A', '<' ->  "v<<A"
    | 'A', '>' ->  "vA"
    | a, b when a = b -> "A"
    | _ -> assert false
    ) in
    let l = if depth = 1 then
      Seq.length seq
    else
      best_arrowpad_seq seq (depth-1) in
    Hashtbl.add cached_arrowpad_move move l;
    l in

  let rec seq_to_len seq = match seq with
  | (s::e::rest) -> (best_arrowpad_move s e) :: seq_to_len (e::rest)
  | _ -> [] in

  seq
  |> Seq.cons 'A'
  |> List.of_seq
  |> seq_to_len
  |> List.fold_left (+) 0


  

let numpad_cache = Hashtbl.create 100

let best_numpad_seq seq depth =
  let best_numpad_move (sx, sy) (ex, ey) =
    let move = (sx, sy), (ex, ey) in
    match Hashtbl.find_opt numpad_cache move with
    | Some(l) -> l
    | None -> 
    let (dx, dy) = (ex - sx, ey - sy) in
    let h =
      let c = if dx > 0 then '>' else '<' in
      n_chars c (abs dx) in
    let v =
      let c = if dy > 0 then 'v' else '^' in
      n_chars c (abs dy) in
    let seq = if sx = 0 && ey = 3 then
      Seq.append h v
    else if sy = 3 && ex = 0 then
      Seq.append v h
    else if dx < 0 && dy < 0 then
      Seq.append h v
    else if dx > 0 && dy > 0 then
      Seq.append v h
    else
      Seq.append v h in
    let seq = Seq.append seq (Seq.return 'A') in
    let seq_len = best_arrowpad_seq seq depth in
    Hashtbl.add numpad_cache move seq_len;
    seq_len in

  let rec seq_to_len seq = match seq with
  | (s::e::rest) -> (best_numpad_move s e) :: seq_to_len (e::rest)
  | _ -> [] in

  seq
  |> Seq.cons 'A'
  |> Seq.map numpad_coord
  |> List.of_seq
  |> seq_to_len
  |> List.fold_left (+) 0



let part1 txt =
  let inputs = String.split_on_char '\n' txt in
  inputs
  |> List.map (fun i -> (String.sub i 0 (String.index i 'A') |> int_of_string, String.to_seq i))
  |> List.map (fun i -> (fst i, best_numpad_seq (snd i) 2))
  |> List.map (fun (num, len) -> num * len)
  |> List.fold_left (+) 0
  |> string_of_int
  
let part2 txt =
  let inputs = String.split_on_char '\n' txt in
  inputs
  |> List.map (fun i -> (String.sub i 0 (String.index i 'A') |> int_of_string, String.to_seq i))
  |> List.map (fun i -> (fst i, best_numpad_seq (snd i) 25))
  |> List.map (fun (num, len) -> num * len)
  |> List.fold_left (+) 0
  |> string_of_int
    
  
