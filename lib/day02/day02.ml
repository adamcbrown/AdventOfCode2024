let (|>) v f = f v

let process_input txt =
  let process_line line =
    String.split_on_char ' ' line
    |> List.map int_of_string
  in
  String.split_on_char '\n' txt
  |> List.map process_line


  let diffs list =
    let rec helper acc list =
      match list with
      | (x :: y :: rest) -> helper ((x-y)::acc) (y::rest)
      | _ -> acc
    in
    helper [] list

  let diffs_are_safe deltas =
    let valid i = match Seq.uncons deltas with
      | Some(hd, _) -> if hd > 0 then i == 1 || i == 2 || i == 3 else i == (-1) || i == (-2) || i == (-3)
      | None -> raise (invalid_arg "empty sequence")
    in
    Seq.for_all valid deltas

  
  let seq_skip_i seq n =
    seq
    |> Seq.mapi (fun i v -> (i, v)) 
    |> Seq.filter (fun (i, _) -> i != n)
    |> Seq.map (fun (_, v) -> v)

  let diffs_are_safe_with_tolerance seq =
    seq :: List.init (Seq.length seq) (fun n -> seq_skip_i seq n)
    |> List.map List.of_seq
    |> List.map diffs
    |> List.map List.to_seq
    |> List.exists diffs_are_safe
      

  let part1 txt =
    txt
    |> process_input
    |> List.map diffs
    |> List.map List.to_seq
    |> List.filter diffs_are_safe 
    |> List.length
    |> string_of_int
    

  let part2 txt =
    txt
    |> process_input
    |> List.map List.to_seq
    |> List.filter diffs_are_safe_with_tolerance
    |> List.length
    |> string_of_int