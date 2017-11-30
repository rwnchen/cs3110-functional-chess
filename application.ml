open Board
(* open Replayer *)
(* open Controller *)
open Opener

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)
(* type metadata = tag_pair list

type game = board * metadata * bool *)

type state = (board * color)

type position = int * int

type move = position * position

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Helper functions/states *)

(* game -> board *)
let extract_board g = failwith "extract_board unimplemented"

(* game -> bool
 * true if it is white's turn *)
let extract_turn g = failwith "extract_turn unimplemented"

(* game -> metadata *)
let extract_meta g = failwith "extract_meta unimplemented"

(* metadata -> tag_pair list *)
let extract_tags m = failwith "extract_tags unimplemented"

(* TODO: *)
let parse_space s =
  let fst_int = Char.code (String.get s 0) - 64 in
  let snd_int = int_of_char (String.get s 1) in
  (fst_int, snd_int)
  (* Char.code(c) - 64 *)

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)
let initial_state = 42

let suggest_moveset o g = []

let suggest_move o g = None

(* let to_replay = failwith "to_replay unimplemented" *)

let rec run (b,c) = begin
  let legal_moves = legal_moves b Board.mh c in
  match legal_moves with
  | [] -> () (*CHECK ENDGAME*)
  | _ ->
    let input = read_line () in
    let spaces = (String.split_on_char ' ' input) in
    let pos1 = parse_space (List.nth spaces 0) in
    let pos2 = parse_space (List.nth spaces 1) in
    (*TODO: parse spaces into positions*)
    let (new_b, check) = make_move b c (pos1,pos2) legal_moves in
    (* give user feedback about move *)
    match c with
    | Black ->
      print_endline "Black Moved";
      run (new_b, White)
    | White ->
      print_endline "White Moved";
      run (new_b, Black)
end

let () =
  let b = init_board in
  let c = White in
  run (b,c)
