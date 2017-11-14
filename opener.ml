open Str
(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)

(* TODO: Remove this once the game/board engine types have been defined *)
(* Represents the current state of the game board *)
type board

(* Metadata associated with each opening, such as white/black winrates etc.
 *)
type metadata =
  {
    w_winrate : float; (* White win rate *)
    name : string;     (* ECO name *)
    category : string; (* ECO category *)
  }

(* Represents an actual opening move SEQUENCE
 * TODO: Find a unified representation type for chess moves, other than [string] *)
type opmoves = metadata * string

(* Represents the database of stored openings
 *
 * We will have to find some way of saving this database beforehand and loading
 * it every time the application begins. *)
type openings

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Helper functions/states *)

(* Regex used to parse string lines containing the game moves. *)
let moves_regex = regexp "."

(* Regex used to parse *)

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let suggest_moveset o b = []

let suggest_move o b = None

let opening_name o = failwith "opening_name: Unimplemented"

