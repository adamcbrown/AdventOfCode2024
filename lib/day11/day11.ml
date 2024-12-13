let add tbl stone count = 
  match Hashtbl.find_opt tbl stone with
  | None -> Hashtbl.replace tbl stone count
  | Some(v) -> Hashtbl.replace tbl stone (v+count)

let load_data txt =
  let tbl = Hashtbl.create 100 in 
  txt
  |> String.split_on_char ' '
  |> List.map int_of_string
  |> List.iter (fun stone -> add tbl stone 1);
  tbl
  

let rec do_n n fn init = match n with
| 0 -> init
| n -> print_int n; print_newline(); do_n (n-1) fn (fn init)

let rec num_digits n =
  if n < 10 then 1 else 1+(num_digits (n/10))

let rec pow10 n = match n with
| 1 -> 10
| n -> 10 * pow10 (n-1)

let split n digits =
  let k = pow10 digits in
  (n / k, n mod k)

let tf_stones stones =
  let new_stones = Hashtbl.create ((Hashtbl.length stones) * 2) in
  let add = add new_stones in

  let tf_stone stone count =
    match stone with
    | 0 -> add 1 count
    | _ ->
      let num = num_digits stone in
      if num mod 2 == 1 then
        add (stone*2024) count
      else
        let left, right = split stone (num/2) in
        add left count; add right count in
  Hashtbl.iter tf_stone stones;
  new_stones
  (* match stones with
  | (stone::rest) -> (match stone with
    | 0 -> 1 :: tf_stones rest
    | x ->
      match num_digits x with
      | n when n mod 2 == 1 -> x*2024 :: tf_stones rest
      | n ->
        let left, right = split x (n/2) in
        left :: right :: tf_stones rest)
  | [] -> [] *)
    
  
let part1 txt =
  txt
  |> load_data
  |> do_n 25 tf_stones
  |> Hashtbl.to_seq_values
  |> Seq.fold_left (+) 0
  |> string_of_int

let part2 txt =
  txt
  |> load_data
  |> do_n 75 tf_stones
  |> Hashtbl.to_seq_values
  |> Seq.fold_left (+) 0
  |> string_of_int

  
