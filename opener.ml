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

(* Trie database *)
module StrMap = Map.Make(String)
module StrTrie = Make(StrMap)
let init_database = StrTrie.empty (* Empty database *)

(* Regex used to parse string lines containing the game moves. *)
let moves_regex = regexp "."



(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f =
  let replays = load_png f in
  let 

let opening_name o = failwith "opening_name: Unimplemented"

