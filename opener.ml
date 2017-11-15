open Str
open Trie
open Board
(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)

(* Metadata associated with each opening, such as white/black winrates etc.
 *)
type opmetadata =
  {
    w_winrate : float; (* White win rate *)
    name : string;     (* ECO name *)
    category : string; (* ECO category *)
  }

type opmoves = opmetadata * move

type openings = ()

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Helper functions/states *)

(* Regex used to parse string lines containing the game moves. *)
let moves_regex = regexp "."

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f = failwith "init_openings: Unimplemented"

let opening_name o = failwith "opening_name: Unimplemented"

