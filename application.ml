open Board
(* open Replayer *)
(* open Controller *)
(* open Opener *)

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)
(* type metadata = tag_pair list

type game = board * metadata * bool *)

<<<<<<< HEAD
type state = board * color * (piece * move)
=======
type state = (board * color)

type position = int * int

type move = position * position
>>>>>>> a68df067e05aa64cb5392b9ad6c05c18eb6affbd

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
(*
   These two functions convert to and from algebraic chess notation to/from the
   moves representation currently used by board.
   Two signatures:
     move -> (game state sth sth) -> string
     string -> (game state sth sth) -> move

   The game state is needed because algebraic chess notation is minimalistic, e.g.
   if only 1 piece can move to a square e5, then the notation will simply be "e5".
   But if two or more pieces can, then a letter will be prefixed to indicate which
   piece does the move e.g. (B for bishop) "Be5". This requires knowing what pieces
   are where and where they can move to.
*)
let to_algnotation m g = failwith "to_algnotation unimplemented"
let from_algnotation m g = failwith "from_algonotation unimplemented"

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
    let brd = print_board new_b in
    match c with
    | Black ->
      print_endline brd;
      (* print_endline "Black Moved"; *)
      run (new_b, White)
    | White ->
      print_endline brd;
      (* print_endline "White Moved"; *)
      run (new_b, Black)
end

let () =
  let b = init_board in
  let c = White in
  run (b,c)
