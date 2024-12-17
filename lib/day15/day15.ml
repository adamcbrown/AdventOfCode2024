module IntPairSet = Set.Make(struct 
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | v -> v
end);;


let print (x, y) = print_int x; print_char ' '; print_int y

type state = {
  rocks: IntPairSet.t;
  walls: IntPairSet.t;
  player: int * int
}

let split_on_substring substr str =
  let list = ref [] in
  let last_idx = ref 0 in
  let split_size = String.length substr in
  for i = 0 to String.length str-1-split_size do
    if String.sub str i split_size = substr then (
      list := String.sub str !last_idx (i - !last_idx) :: !list;
      last_idx := i + split_size;
    )
  done;
  list := String.sub str !last_idx (String.length str - !last_idx) :: !list;
  List.rev !list

let load_data txt =
  match split_on_substring "\n\n" txt with
  | mapTxt :: instrTxt :: [] ->
    let load_map txt =
      let load_row state (y, row) =
        let load_col state (x, c)  = match c with
        | '#' -> {state with walls = IntPairSet.add (x, y) state.walls}
        | 'O' -> {state with rocks = IntPairSet.add (x, y) state.rocks}
        | '@' -> {state with player = (x, y)}
        | _ -> state in
        row
        |> String.to_seq
        |> Seq.mapi (fun i c -> (i, c))
        |> Seq.fold_left load_col state in
      txt
      |> String.split_on_char '\n'
      |> List.mapi (fun i c -> (i, c))
      |> List.fold_left load_row {rocks = IntPairSet.empty; walls = IntPairSet.empty; player = (0,0)} in
    let load_instr c = match c with
    | '^' -> Some (0, -1)
    | '>' -> Some (1, 0)
    | 'v' -> Some (0, 1)
    | '<' -> Some (-1, 0)
    | _ -> None in
    load_map mapTxt, instrTxt |> String.to_seq |> (Seq.filter_map load_instr) |> List.of_seq
  | _ -> raise (invalid_arg "bad")

let shift (x1, y1) (dx, dy) = (x1 + dx, y1 + dy)
   

let part1 txt =
  let apply state instr =
    let next_pos = shift state.player instr in
    if IntPairSet.mem next_pos state.walls then state
    else if not (IntPairSet.mem next_pos state.rocks) then
      {state with player = next_pos}
    else
      let rec rock_helper rock_pos =
        let next_pos = shift rock_pos instr in
        if IntPairSet.mem next_pos state.walls then
          None
        else if IntPairSet.mem next_pos state.rocks then
          rock_helper next_pos
        else
          Some next_pos in
      match rock_helper next_pos with
      | Some(empty_pos) ->
        {state with player = next_pos; rocks = state.rocks |> IntPairSet.remove next_pos |> IntPairSet.add empty_pos }
        | None -> state in
  let init_state, instrs = load_data txt in
  instrs
  |> List.fold_left apply init_state
  |> (fun s -> s.rocks)
  |> IntPairSet.to_seq
  |> Seq.map (fun (x, y) -> 100 * y + x)
  |> Seq.fold_left (+) 0
  |> string_of_int

let left_rock_pos (x, y) = (x, y)
let right_rock_pos (x, y) = (x-1, y)


let part2 txt =
  let tf_for_part_2 state =
    let walls =
      state.walls
      |> IntPairSet.to_seq
      |> Seq.flat_map (fun (x, y) -> Seq.cons (2*x, y) (Seq.cons (2*x+1, y) Seq.empty))
      |> IntPairSet.of_seq in
    let rocks = IntPairSet.map (fun (x, y) -> (2*x, y)) state.rocks in
    let player = let (x, y) = state.player in (2*x, y) in
    {player; rocks; walls} in
  let apply state (dx, dy) =
    if IntPairSet.mem (shift state.player (dx, dy)) state.walls then state
    else if dx != 0 then (
      let rock_pos = if dx=(-1) then (shift state.player (-2, 0)) else (shift state.player (1, 0)) in
      if not (IntPairSet.mem rock_pos state.rocks) then {state with player = shift state.player (dx, 0)} else
      let rec rock_helper rock_pos =
        let wall_pos = if dx = (-1) then (shift rock_pos (-1, 0)) else (shift rock_pos (2, 0)) in
        let next_rock_pos = shift rock_pos (2*dx, 0) in
        if IntPairSet.mem wall_pos state.walls then
          None
        else if IntPairSet.mem next_rock_pos state.rocks then
          match rock_helper next_rock_pos with
          | Some(ls) -> Some(rock_pos :: ls)
          | None -> None
        else
          Some [rock_pos] in
      match rock_helper rock_pos with
      | Some(moved_rocks) ->
        let rocks =
          state.rocks
          |> List.fold_right IntPairSet.remove moved_rocks
          |> List.fold_right IntPairSet.add (List.map (shift (dx, 0)) moved_rocks) in
        let player = shift state.player (dx, dy) in
        {state with player; rocks}
      | None -> state
    )else if dy != 0 then(
      let rec rock_helper rock_pos =
        if IntPairSet.mem (shift rock_pos (0, dy)) state.walls || IntPairSet.mem (shift rock_pos (1, dy)) state.walls then
          None
        else
          let check_rock dv =
            let new_pos = shift rock_pos dv in
            if IntPairSet.mem new_pos state.rocks then rock_helper new_pos else Some Seq.empty in

          match check_rock ((-1), dy), check_rock (0, dy), check_rock (1, dy) with
          | Some(s1), Some(s2), Some(s3) ->
            let lower_rocks = Seq.append (Seq.append s1 s2) s3 in
            Some (Seq.cons rock_pos lower_rocks)
          | _ -> None in

      let check_for_rock dv =
        let pos = shift state.player dv in
        if IntPairSet.mem pos state.rocks then rock_helper pos else Some Seq.empty in
      match check_for_rock (0, dy), check_for_rock (-1, dy) with
      | Some(s1), Some(s2) ->
        let moved_rocks =
          Seq.append s1 s2
          |> IntPairSet.of_seq
          |> IntPairSet.to_seq
          |> List.of_seq in
        let rocks =
          state.rocks
          |> List.fold_right IntPairSet.remove moved_rocks
          |> List.fold_right IntPairSet.add (List.map (shift (dx, dy)) moved_rocks) in
        let player = shift state.player (0, dy) in
        {state with rocks; player}
      | _ -> state
    )else
      raise (invalid_arg "bad dir") in
  let init_state, instrs = load_data txt in
  let init_state = tf_for_part_2 init_state in
  instrs
  |> List.to_seq
  (* |> Seq.take 1 *)
  |> Seq.fold_left apply init_state
  (* |> (fun i -> apply init_state i) *)
  |> (fun s -> s.rocks)
  |> IntPairSet.to_seq
  (* |> Seq.iter (fun (x, y) -> print_int x; print_char ' '; print_int y; print_newline()); "" *)
  |> Seq.map (fun (x, y) -> 100 * y + x)
  |> Seq.fold_left (+) 0
  |> string_of_int






