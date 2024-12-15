let load_data txt =
  let is_num c = int_of_char c >= int_of_char '0' && int_of_char c <= int_of_char '9' || c == '-' in
  let parse_robot line =
    let seq = line
    |> String.to_seq
    |> Seq.drop 2 in
    let px = Seq.take_while is_num seq |> String.of_seq |> int_of_string in
    let seq =
      seq
      |> Seq.drop_while is_num
      |> Seq.drop 1 in
    let py = Seq.take_while is_num seq |> String.of_seq |> int_of_string in
    let seq =
      seq
      |> Seq.drop_while is_num
      |> Seq.drop 3 in
    let vx = Seq.take_while is_num seq |> String.of_seq |> int_of_string in
    let vy =
      seq
      |> Seq.drop_while is_num
      |> Seq.drop 1
      |> Seq.take_while is_num
      |> String.of_seq
      |> int_of_string in
    (px, py, vx, vy) in
  match String.split_on_char '\n' txt with
  | w::h::rest -> int_of_string w, int_of_string h, (List.map parse_robot rest)
  | _ -> raise (invalid_arg "unknown")
  

let partition n fn list =
  let arr = Array.make n [] in
  List.iter (fun elem -> arr.(fn elem) <- elem::arr.(fn elem)) list;
  Array.to_list arr

let part1 txt =
  let w, h, robots = load_data txt in
  let iterate_robot (px, py, vx, vy) =
    let x, y = (px + vx * 100) mod w, (py + vy * 100) mod h in
    (if x >= 0 then x else x+w),(if y >= 0 then y else y+h) in
  let quadrant (px, py) =
    if px < w/2 && py < h/2 then 1
    else if px < w/2 && py > h/2 then 2
    else if px > w/2 && py < h/2 then 3
    else if px > w/2 && py > h/2 then 4
    else 0 in
      
  robots
  |> List.map iterate_robot
  |> partition 5 quadrant
  |> List.tl
  |> List.map List.length
  |> List.fold_left ( * ) 1
  |> string_of_int

module IntPairSet = Set.Make(struct 
type t = int * int
let compare (a1, a2) (b1, b2) =
  match compare a1 b1 with
  | 0 -> compare a2 b2
  | v -> v
end);;


let part2 txt =
  let w, h, robots = load_data txt in
  let iterate_robot dt (px, py, vx, vy) =
    let x, y = (px + vx * dt) mod w, (py + vy * dt) mod h in
    (if x >= 0 then x else x+w),(if y >= 0 then y else y+h),vx,vy in
    
  let rec loop frame robots =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
    Raylib.begin_drawing();
    Raylib.clear_background Raylib.Color.black;
    List.iter (fun (x, y, _, _) -> Raylib.draw_pixel x y Raylib.Color.white) robots;
    Raylib.draw_text (string_of_int frame) 0 0 12 Raylib.Color.beige;
    Raylib.end_drawing();
    if Raylib.is_key_down Raylib.Key.Right then
      loop (frame+1) (List.map (iterate_robot 1) robots)
    else if Raylib.is_key_down Raylib.Key.Left then
      loop (frame-1) (List.map (iterate_robot (-1)) robots)
    else if Raylib.is_key_pressed Raylib.Key.Space then 
      loop (frame+100) (List.map (iterate_robot 100) robots)
    else
      loop frame robots in

  Raylib.init_window w h "inspector";
  Raylib.set_target_fps 20;
  loop 0 robots