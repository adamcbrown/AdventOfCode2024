type token =
| LParen
| RParen
| Comma
| Identifier of string
| Int of int
| Unknown of char
| End

type instruction =
| Mul of int * int
| DoCall
| DontCall

module Lexer = struct
  let is_id_part c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '\''
  let is_num c = (c >= '0' && c <= '9')

  let lex_while cs pred =
    let rec helper cs = match cs with
    | (c::cs) when pred c ->
      let (matched, unmatched) = helper cs in
      (c :: matched, unmatched)
    | cs -> ([], cs)
    in
    let matched, unmatched = helper cs in
    (matched |> List.to_seq |> String.of_seq), unmatched
  
      
  let lex_word cs =
    let matched, unmatched = lex_while cs is_id_part in
    Identifier matched, unmatched
  
  let lex_num cs =
    let matched, unmatched = lex_while cs is_num in
    Int (int_of_string matched), unmatched
  
  let next cs =
    match cs with
    | (x::_) when is_id_part x -> Some (lex_word cs)
    | (x::_) when is_num x -> Some (lex_num cs)
    | (x::xs) when x = '(' -> Some (LParen, xs)
    | (x::xs) when x = ')' -> Some (RParen, xs)
    | (x::xs) when x = ',' -> Some (Comma, xs)
    | (x::xs) -> Some (Unknown x, xs)
    | [] -> None
end

module Parser = struct
  let rec next tokens =
    let ends_with s id = String.ends_with ~suffix:s id in
    match tokens with
    | (Identifier(id)::LParen::Int(n1)::Comma::Int(n2)::RParen::rest) when (ends_with "mul" id) -> Some (Mul(n1, n2), rest)
    | (Identifier(id)::LParen::RParen::rest) when (ends_with "do" id) -> Some (DoCall, rest)
    | (Identifier(id)::LParen::RParen::rest) when (ends_with "don't" id) -> Some(DontCall, rest)
    | (_::xs) -> next xs
    | [] -> None
end


(* All recursively builds out a list of outputs via the next_fn *)
let all next_fn inputs =
  let rec helper inputs = match next_fn inputs with
  | Some(next_val, rest) -> next_val :: helper rest
  | None -> []
  in
  helper inputs

(* Process input converts the raw text into a list of instructions *)
let process_input txt =
  txt
  |> String.to_seq
  |> List.of_seq
  |> all Lexer.next
  |> all Parser.next


(* For part 1, we evaluate all the multiply instructins *)
type machine_state_1 = {
  value: int
}

let part1 txt =
  let eval state instr = match instr with
    | Mul(a, b) -> {value = state.value + a*b}
    | _ ->  state
  in
  txt
  |> process_input
  |> List.fold_left eval {value = 0}
  |> fun s -> s.value
  |> string_of_int


(*
  For part 2, we evaluate multiply instructions respecting do() and don't() calls which
  enable and disable executing multiply instructions respectively
*)
type machine_state_2 = {
  value: int;
  on: bool
}

let part2 txt =
  let eval state instr = match instr with
    | Mul(a, b) -> if state.on then {state with value = state.value + a*b} else state
    | DoCall ->  {state with on = true}
    | DontCall ->  {state with on = false}
  in
  txt
  |> process_input
  |> List.fold_left eval {value = 0; on=true}
  |> fun s -> s.value
  |> string_of_int