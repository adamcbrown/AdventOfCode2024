module IntPairSet = Set.Make(struct 
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | v -> v
end);;

module GuardStateSet = Set.Make(struct 
  type t = (int * int) * (int * int)
  let compare (p1, d1) (p2, d2) =
    let cmp_pair (a1, a2) (b1, b2) =
      match compare a1 b1 with
      | 0 -> compare a2 b2
      | v -> v in
    match cmp_pair p1 p2 with
    | 0 -> cmp_pair d1 d2
    | v -> v
end);;

type map = {
  guard_pos: int*int;
  guard_dir: int*int;
  boxes: (int*int) list;
  w: int;
  h: int
}
[@@deriving show]

let load_map txt =
  let lines = String.split_on_char '\n' txt in
  let h = List.length lines and w = String.length (List.hd lines) in
  let map = {guard_pos = (0,0); guard_dir = (0,0); boxes = []; w; h} in

  let load_row map (y, row) =
    let load_col map (x, c) = match c with
    | '.' -> map
    | '>' -> {map with guard_pos = (x, y); guard_dir = (1, 0)}
    | '<' -> {map with guard_pos = (x, y); guard_dir = (-1, 0)}
    | 'V' -> {map with guard_pos = (x, y); guard_dir = (0, 1)}
    | '^' -> {map with guard_pos = (x, y); guard_dir = (0, -1)}
    | '#'-> {map with boxes = (x, y)::map.boxes}
    | _ -> raise (invalid_arg "unknown char found") in
    row
    |> String.to_seq
    |> List.of_seq
    |> List.mapi (fun x col -> (x, col))
    |> List.fold_left load_col map in
  txt
  |> String.split_on_char '\n'
  |> List.mapi (fun y row -> (y, row))
  |> List.fold_left load_row map

let pair_add (a, b) (c, d) = (a+c), (b+d)

let rotate (a, b) = (-b, a)


let get_positions map =
  let rec iterate positions map =
    match map.guard_pos with
    | (x, y) when x < 0 || x >= map.w || y < 0 || y >= map.h ->
      positions, map
    | pos when List.mem (pair_add pos map.guard_dir) map.boxes ->
      iterate positions {map with guard_dir = rotate map.guard_dir}
    | pos ->
      iterate (IntPairSet.add pos positions) {map with guard_pos = pair_add pos map.guard_dir} in
  let positions, _ = iterate IntPairSet.empty map in
  positions

let part1 txt =
  txt
  |> load_map
  |> get_positions
  |> IntPairSet.cardinal
  |> string_of_int
  
let part2 txt =
  let i = ref 0 in
  let make_maps positions map = List.map (fun pos -> {map with boxes = pos :: map.boxes}) positions in
  let rec is_loop guard_states map =
    if GuardStateSet.mem (map.guard_pos, map.guard_dir) guard_states then true
    else
      match map.guard_pos with
      | (x, y) when x < 0 || x >= map.w || y < 0 || y >= map.h ->
        false
      | pos when List.mem (pair_add pos map.guard_dir) map.boxes ->
        is_loop guard_states {map with guard_dir = rotate map.guard_dir}
      | pos ->
        is_loop (GuardStateSet.add (pos, map.guard_dir) guard_states) {map with guard_pos = pair_add pos map.guard_dir} in
  let test map =
    i := !i+1;
    if !i mod 10 = 0 then (print_int !i; print_newline());
    is_loop GuardStateSet.empty map in
  let map = load_map txt in
  let positions =
    map
    |> get_positions
    |> IntPairSet.remove map.guard_pos
    |> IntPairSet.to_list in
  print_int (List.length positions); print_newline();
  map
  |> make_maps positions
  |> List.filter test
  |> List.length
  |> string_of_int
      