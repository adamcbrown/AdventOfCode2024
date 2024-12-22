let next_number n =
  let mix_and_prune fn n = (Int.logxor n (fn n)) mod 16777216 in
  n
  |> mix_and_prune (fun i -> i * 64)
  |> mix_and_prune (fun i -> i / 32)
  |> mix_and_prune (fun i -> i * 2048)

let rec next_number_seq init =
  fun () -> Seq.Cons (init, next_number_seq (next_number init))

let part1 txt =
  let get_nth n seq =
    seq
    |> Seq.drop n
    |> Seq.take 1
    |> Seq.uncons
    |> Option.get
    |> fst in
  txt
  |> String.split_on_char '\n'
  |> List.map int_of_string
  |> List.map next_number_seq
  |> List.map (get_nth 2000)
  |> List.fold_left (+) 0
  |> string_of_int

let rec compute_diffs xs =
  match xs with
  | a::b::rest -> (b-a) :: compute_diffs (b::rest)
  | _ -> []

let rec build_sequence_to_bananas init_banana diffs =
  match diffs with
  | (a::b::c::d::rest) -> ((a,b,c,d), init_banana + a + b + c + d) :: (build_sequence_to_bananas (init_banana + a) (b::c::d::rest))
  | _ -> []


let part2 txt =

  let build_diffs_to_bananas init_banana = 
    init_banana
    |> next_number_seq
    |> Seq.take 2001
    |> List.of_seq
    |> List.map (fun i -> i mod 10)
    |> compute_diffs
    |> build_sequence_to_bananas (init_banana mod 10)
    |> List.rev
    |> List.to_seq
    |> Hashtbl.of_seq in

  let diffs_to_bananas =
    txt
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> List.map build_diffs_to_bananas in

  let num_bananas diffs =
    diffs_to_bananas
    |> List.filter_map (fun dtb -> Hashtbl.find_opt dtb diffs)
    |> List.fold_left (+) 0 in

  let all_keys =
    diffs_to_bananas
    |> List.map Hashtbl.to_seq_keys
    |> List.map List.of_seq
    |> List.concat
    |> List.sort_uniq compare in

  all_keys
  |> List.map num_bananas
  |> List.fold_left max Int.min_int
  |> string_of_int
  