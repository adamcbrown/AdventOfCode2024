let load txt =
  let lines =
    txt
    |> String.split_on_char '\n'
    |> Array.of_list in
  let w, h = String.length lines.(0), Array.length lines in
  let get (x,y) = 
    if x >= 0 && x < w && y >=0 && y < h then
      String.get lines.(y) x
    else
      '.' in
  get, w, h

let part1 txt =
  let get, w, h = load txt in
  let mask = Array.make_matrix w h false in
  let compute (x0,y0) =
    if mask.(x0).(y0) then None else(
      let area, perim = ref 0, ref 0 in
      let rec helper (x,y) =
        if not mask.(x).(y) then(
          mask.(x).(y) <- true;
          area := !area + 1;
          let c = get(x,y) in
          [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
          |> List.iter (fun pt -> if get pt == c then helper pt else perim := !perim + 1)
        ) in
      helper (x0, y0);
      Some (!area * !perim)
    )in
  Seq.ints 0
  |> Seq.take (w*h)
  |> Seq.map (fun i -> (i mod w, i/w))
  |> Seq.filter_map compute
  |> Seq.fold_left (+) 0
  |> string_of_int


  let part2 txt =
    let get, w, h = load txt in
    let mask = Array.make_matrix w h false in
    let compute (x0,y0) =
      if mask.(x0).(y0) then None else(
        let area, corners = ref 0, ref 0 in
        let rec helper (x,y) =
          if not mask.(x).(y) then(
            mask.(x).(y) <- true;
            area := !area + 1;
            let c = get(x,y) in
            let num_corners_at_pt =
              let is_corner (dx, dy) = match c == get(x+dx, y+dy), c == get(x+dx, y), c == get(x, y+dy) with
                | _, false, false -> true
                | false, true, true -> true
                | _ -> false in
              [(-1,-1);(-1,1);(1,1);(1,-1)]
              |> List.filter is_corner
              |> List.length in
            corners := !corners + num_corners_at_pt;
            [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
            |> List.iter (fun pt -> if get pt == c then helper pt)
          ) in
        helper (x0, y0);
        Some (!area * !corners)
      )in
    Seq.ints 0
    |> Seq.take (w*h)
    |> Seq.map (fun i -> (i mod w, i/w))
    |> Seq.filter_map compute
    |> Seq.fold_left (+) 0
    |> string_of_int
  
