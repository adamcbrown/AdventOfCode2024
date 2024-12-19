type state = {
  output: int -> bool;
  mem: int array;

  a: int;
  b: int;
  c: int;
  ip: int;
}

let load_data txt output =
  let relevent_data line =
    let start = String.index line ':' + 2 in
    let len = String.length line - start in
    String.sub line start len in
  
  let data = txt |> String.split_on_char '\n' |> Array.of_list in
  {
    output;
    mem = data.(4) |> relevent_data |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list;
    a = data.(0) |> relevent_data |> int_of_string;
    b = data.(1) |> relevent_data |> int_of_string;
    c = data.(2) |> relevent_data |> int_of_string;
    ip = 0;
  }



let iterate state =
  if state.ip < 0 || state.ip >= Array.length state.mem then None else

  let combo_op v = match v with
  | 4 -> state.a
  | 5 -> state.b
  | 6 -> state.c
  | 7 -> raise (invalid_arg "7 in combo op")
  | v -> v in

  let instr, op = state.mem.(state.ip), state.mem.(state.ip + 1) in
   match instr with
  | 0 -> Some {state with a = state.a / (Int.shift_left 1 (combo_op op)); ip = state.ip + 2}
  | 1 -> Some {state with b = Int.logxor state.b op; ip = state.ip + 2}
  | 2 -> Some {state with b = (combo_op op) mod 8; ip = state.ip + 2}
  | 3 -> Some {state with ip = if state.a = 0 then state.ip+2 else op}
  | 4 -> Some {state with b = Int.logxor state.b state.c; ip = state.ip + 2}
  | 5 -> if state.output ((combo_op op) mod 8) then Some {state with ip = state.ip + 2} else None
  | 6 -> Some {state with b = state.a / (Int.shift_left 1 (combo_op op)); ip = state.ip + 2}
  | 7 -> Some {state with c = state.a / (Int.shift_left 1 (combo_op op)); ip = state.ip + 2}
  | _ -> raise (invalid_arg "bad opcode")

let rec iterate_until_none fn init =
  match fn init with
  | Some(next) -> iterate_until_none fn next
  | None -> ()
  

let part1 txt =
  let outputs = ref [] in
  let add_output i = outputs := i :: !outputs; true in
  load_data txt add_output
  |> iterate_until_none iterate;
  !outputs
  |> List.rev
  |> List.map string_of_int
  |> String.concat ","


let part2 txt =
  
  let outputs = ref [] in
  let add_output i = outputs := i :: !outputs; true in
  let state = load_data txt add_output in

  let want = state.mem in

  let get_output i =
    outputs := [];
    iterate_until_none iterate {state with a = i};
    !outputs in

  let valid_suffix list = 
    list
    |> List.mapi (fun i v -> want.(Array.length want - 1 - i) = v)
    |> List.for_all (fun v -> v) in

  let _print i v =
    print_int i; print_string ": "; List.iter (fun i -> print_int i; print_char ',') v; print_newline()in

  let rec search i =
    let results =
      Seq.ints i
      |> Seq.take 8
      |> Seq.map (fun i -> (i, get_output i))
      |> Seq.filter (fun (_, res) -> valid_suffix res) in

    match Seq.uncons results with
    | Some((i, v), _) when List.length v = Array.length want -> (Some i)
    | None -> None
    | Some(_) ->
        results
        |> Seq.filter (fun (k, _) -> k != 0)
        |> Seq.map (fun (i, _) -> i * 8)
        |> Seq.filter_map search
        |> Seq.uncons
        |> Option.map (fun (i, _) -> i) in

  search 0
  |> Option.get
  |> string_of_int



