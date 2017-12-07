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

(* [parse_space s]
 * Parses user-inputted spaces to our representation of a board position.
 * [s]: a string of the position,  *)
let parse_space s =
  let s' = String.uppercase_ascii s in
  let fst_int = Char.code (String.get s' 0) - 64 in
  let snd_int = int_of_char (String.get s' 1) - 48 in
  (fst_int, snd_int)

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)
let initial_state = 42

let suggest_moveset o g = []

let suggest_move o g = None

(* let to_replay = failwith "to_replay unimplemented" *)

(* [print_tup ppf (a,b)]
 * Printing function for tuples. Debug purposes. *)
let print_tup ppf (a,b) =
  Format.fprintf ppf "(%d,%d)" a b

(* [print_mov ppf ((a,b),(c,d))]
 * Printing function for moves. Debug purposes. *)
let print_mov ppf ((a,b),(c,d)) =
  Format.fprintf ppf "((%d,%d) to (%d,%d))" a b c d

(* [print_mov ppf ((a,b),(c,d))]
 * Printing function for the last move on a board. Debug purposes. *)
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
        | Pawn (m,_) -> "Pawn: " ^ (string_of_bool m) in
    Format.fprintf ppf "Last Move: (%s // (%d,%d) to (%d,%d))" piece (fst i) (snd i) (fst f) (snd f)
  | None -> ()

(* [run (b,c,lm)]
 * Run one round of the REPL, which involves the current color making one move.
 * The moving color switches between rounds.
 * [b]: the current board
 * [c]: the currently moving color
 * [lm]: the last move made on the board *)
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
            let algno = (to_algno lm b (pos1,pos2)) in
            let promote_target = ref None in
            Format.printf "Alg. notation: %s\n" algno;
            Format.printf "Reverse Alg.: %a\n" print_mov (from_algno promote_target lm b algno);
            print_newline ();
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
