open Board
open Replayer
open Controller
open Opener

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)
type metadata = tag_pair list

type game = board * metadata * bool

type state = int (* TODO *)

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

let to_replay = failwith "to_replay unimplemented"

let run s = ()
