let load_data txt =
  let digits = 
    txt
    |> String.to_seq
    |> Seq.map (fun c -> int_of_char c - int_of_char '0') in
  let size = Seq.fold_left (+) 0 digits in
  let data = Array.init size (fun _ -> None) in
  let ptr = ref 0 in
  let update i size =
    (if i mod 2 == 0 then
      let file = i / 2 in
      for j = 0 to size-1 do
        data.(!ptr+j) <- (Some file)
      done);
    ptr := !ptr + size in
  Seq.iteri update digits;
  data

  let rec find_rev_from arr i pred =
    if i == 0 then None
    else if pred arr.(i-1) then (Some (i-1))
    else find_rev_from arr (i-1) pred

  let rec find_from arr i pred =
    if i == (Array.length arr) then None
    else if pred arr.(i) then (Some i)
    else find_from arr (i+1) pred

let debug_print i =
  print_char (match i with
  | None -> '.'
  | Some(v) -> char_of_int (v + int_of_char '0'))

let part1 txt =
  let data = load_data txt in
  let rec defrag head tail =
    match find_rev_from data tail Option.is_some with
    | None -> ()
    | Some(new_tail) -> match find_from data head Option.is_none with
      | Some(new_head) ->
        (if new_head < new_tail then
        let tmp = data.(new_head) in
        data.(new_head) <- data.(new_tail);
        data.(new_tail) <- tmp;
        defrag new_head new_tail)
      | None -> raise (invalid_arg "unknown") in
  defrag 0 (Array.length data);
  let to_val i = match i with | Some(v) -> v | None -> 0 in
  data
  |> Array.mapi (fun i v -> i * to_val v)
  |> Array.fold_left (+) 0
  |> string_of_int

  

  let part2 txt =
    let data = load_data txt in
    let find_n_nones n =
      let rec helper i =
        match find_from data i Option.is_none with
        | None -> None
        | Some(i) ->
          if i >= (Array.length data - n) then None else
          let valid = 
            data
            |> Array.to_seq
            |> Seq.drop i
            |> Seq.take n
            |> Seq.for_all Option.is_none in
          if valid then Some(i) else
          match find_from data i Option.is_some with
          | None -> None
          | Some(i) -> helper i in
      helper 0 in
    let rec defrag file idx =
      print_int idx; print_newline();
      if file == 0 then () else
      match find_rev_from data idx (fun v -> Option.is_some v && Option.get v == file) with
      | None -> raise (invalid_arg "unknown data")
      | Some(file_tail) ->
        let file = Option.get data.(file_tail) in
        match find_rev_from data file_tail (fun v -> Option.is_none v || Option.get v != file) with
        | Some(file_head) ->
          let size = (file_tail - file_head) in
          (match find_n_nones size with
          | None -> ()
          | Some(k) ->
            (if k >= file_head then () else (Array.fill data k size (Some file); Array.fill data (file_head+1) size None));
          );
          (defrag (file-1) (file_head+1))
        | None -> () in
    defrag (Option.get data.(Array.length data - 1)) (Array.length data);
    let to_val i = match i with | Some(v) -> v | None -> 0 in
    data
    |> Array.mapi (fun i v -> i * to_val v)
    |> Array.fold_left (+) 0
    |> string_of_int