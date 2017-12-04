open Format
open Board
(* open Replayer *)
(* open Controller *)
(* open Opener *)

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)
(* type metadata = tag_pair list

type game = board * metadata * bool *)

type state = board * color * ((piece * move) option)

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
  let snd_int = int_of_char (String.get s 1) - 48 in
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

let print_tup ppf (a,b) =
  Format.fprintf ppf "(%d,%d)" a b

let print_lastm ppf (lastm:Board.last_move) =
  match lastm with
  | Some (a,b) ->
    let p = snd a in
    let i = fst b in
    let f = snd b in
    let piece =
        match p with
        | King m -> "King: " ^ (string_of_bool m)
        | Queen -> "Queen"
        | Bishop -> "Bishop"
        | Knight -> "Knight"
        | Rook m -> "Rook: " ^ (string_of_bool m)
        | Pawn m -> "Pawn: " ^ (string_of_bool m) in
    Format.fprintf ppf "Last Move: (%s // (%d,%d) to (%d,%d))" piece (fst i) (snd i) (fst f) (snd f)
  | None -> ()
(*   match lastm with
  | Some (_,p),((i1,i2),(f1,f2)) ->
    begin
      let piece =
        match p with
        | King m -> "King: " ^ (string_of_bool m)
        | Queen -> "Queen"
        | Bishop -> "Bishop"
        | Knight -> "Knight"
        | Rook m -> "Rook: " ^ (string_of_bool m)
        | Pawn m -> "Pawn: " ^ (string_of_bool m) in
      Format.fprintf ppf "Last Move: (%s // (%d,%d) to (%d,%d))" piece i1 i2 f1 f2
    end
  | None -> Format.fprintf ppf "Last Move: None" *)

let rec run (b,c,lm) =
  let leg_moves = legal_moves b lm c in
  match leg_moves with
  | [] -> () (*CHECK ENDGAME*)
  | _ ->
    let input = read_line () in
    let spaces = (String.split_on_char ' ' input) in
    let pos1 = parse_space (List.nth spaces 0) in
    let pos2 = parse_space (List.nth spaces 1) in
    match get_piece b pos1 with
    | Some p ->
      begin
        let (new_b, check) = make_move b c lm (pos1,pos2) leg_moves in
        (* give user feedback about move *)
        if new_b = b
        then
          begin
            print_endline "Not a valid move.";
            Format.printf "%a\n" print_lastm lm;
            run (b, c, lm)
          end
        else
          begin
            let brd = print_board new_b in
            let p' =
              match get_piece new_b pos2 with
              | Some newp -> newp
              | None -> failwith "shouldn't be here" in
            let newlm = Some (p', (pos1,pos2)) in
            print_endline brd;
            Format.printf "%a\n" print_lastm lm;
            run (new_b, (oppc c), newlm)
          end
      end
    | None ->
      begin
         match c with
        | Black ->
          Format.printf "%a\n" print_tup pos1;
          Format.printf "%a\n" print_lastm lm;
          print_endline (print_board b);
          print_endline "No piece selected.";
          (* print_endline "Black Moved"; *)
          run (b, Black, lm)
        | White ->
          Format.printf "%a\n" print_tup pos1;
          Format.printf "%a\n" print_lastm lm;
          print_endline (print_board b);
          print_endline "No piece selected.";
          (* print_endline "White Moved"; *)
          run (b, White, lm)
      end
    (*TODO: parse spaces into positions*)

let () =
  let b = init_board in
  let c = White in
  run (b,c, None)