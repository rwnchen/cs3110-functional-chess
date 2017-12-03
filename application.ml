open Board
open Replayer
open Controller
open Opener

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)
type metadata = tag_pair list

type game = board * metadata * bool

type state = board * color * (piece * move)

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

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)
let initial_state = 42

let suggest_moveset o g = []

let suggest_move o g = None

let to_replay = failwith "to_replay unimplemented"

let run s = ()
  (*


   *)
