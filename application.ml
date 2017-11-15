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


(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)
let initial_state = 42

let extract_board g = failwith "extract_board unimplemented"

let extract_turn g = failwith "extract_turn unimplemented"

let extract_meta g = failwith "extract_meta unimplemented"

let extract_tags m = failwith "extract_tags unimplemented"

let suggest_moveset o g = []

let suggest_move o g = None

let to_replay = failwith "to_replay unimplemented"

let run s = ()
