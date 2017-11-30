open Str
open Trie
open Board
open Yojson.Basic.Util
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

(* *)
let dir = "openings/"
let eco_opening_json = dir ^ "eco_openings.json"
let ficsgames_1 = dir ^ "ficsgames_1.pgn"

let load_png f = None

let init_eco_names f =
  let js = Yojson.Basic.from_channel (open_in f) in
  None

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f =
  let replays = load_png f in
  ()


let opening_name o = failwith "opening_name: Unimplemented"
