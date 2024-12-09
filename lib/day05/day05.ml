module RuleSet = Set.Make(struct 
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | v -> v
end);;

let load txt =
  let lines = String.split_on_char '\n' txt in
  let rec parse_rule rest =
    match List.hd rest with
    | "" -> ([], List.tl rest)
    | rule ->
        match String.split_on_char '|' rule with
        | [before;after] -> 
          let order = (int_of_string before, int_of_string after) in
          let rest, remainder = parse_rule (List.tl rest) in
          (order::rest, remainder)
          | _ -> raise (Invalid_argument rule)
        in
    let rules, updates_txt = parse_rule lines in
    let parse_update line =
      line
      |> String.split_on_char ','
      |> List.map int_of_string
    in
   (RuleSet.of_list rules, List.map parse_update updates_txt)

let rec valid_update in_rule update =
  match update with
  | [] -> true
  | (x::xs) ->
      List.for_all (fun y -> not (in_rule(y,x))) xs &&
      valid_update in_rule xs

let part1 txt =
  let rules, updates = load txt in
  let in_rule (x, y) = RuleSet.mem (x,y) rules in
  let middle xs = List.nth xs (List.length xs / 2) in
  updates
  |> List.filter (valid_update in_rule)
  |> List.map middle
  |> List.fold_left (+) 0
  |> string_of_int


let part2 txt =
  let rules, updates = load txt in
  let in_rule (x, y) = RuleSet.mem (x,y) rules in
  let swap_sort arr =
    for i = 0 to Array.length arr - 1 do
      for j = 0 to Array.length arr - 1 do
        if in_rule (arr.(j), arr.(i)) then
          let x, y = arr.(i), arr.(j) in
          arr.(j) <- x;
          arr.(i) <- y
      done
    done
  in
  updates
  |> List.filter (fun update -> update |> valid_update in_rule |> not)
  |> List.map Array.of_list
  |> List.map (fun updates -> swap_sort updates; updates.(Array.length updates / 2))
  |> List.fold_left (+) 0
  |> string_of_int