open Str
open Board
open Yojson.Basic.Util
open String
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

type openings = ()

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Helper functions/states *)

(* Trie database *)
module StrMap = Map.Make(String)
module StrTrie = Trie.Make(StrMap)
let init_openings = StrTrie.empty (* Empty openings database *)

(* Regex used to parse string lines containing the game moves. *)
let moves_regex = regexp "."

(* *)
let dir = "openings/"
let eco_opening_json = dir ^ "eco_openings.json"
let ficsgames_1 = dir ^ "ficsgames_1.pgn"

(* Ported from a2 - helper function to parse all elements
 * in a Yojson String list to an actual string list *)
let rec json_to_string_list = function
  | [] -> []
  | h::t ->
    (h |> to_string)::(json_to_string_list t)

let load_png f = None

(* [parse_eco f]
 * string -> string list list
 * Given the path of a json file [f], parses [f] into a list of ECO chess
 * openings. Each opening in the list is a string list, with at least 2
 * elements. The first (head) element is the name of the opening, and the
 * following elements are the moves associated with that opening. *)
let parse_eco f =
  let js = Yojson.Basic.from_channel (open_in f) in
  let eco_list_js = js |> member "openings" |> to_list in
  let rec to_eco_list l r =
    match l with
    | [] -> r
    | h::t -> to_eco_list t ((h |> to_list |> json_to_string_list)::r)
  in
  to_eco_list eco_list_js []
  (*List.iter (fun l -> List.iter (fun x -> Printf.printf "%s\t" x) l) eco*)

(* [init_eco f]
 * string -> openings
 * Constructs the openings trie given a json file [f] with all openings and
 * their associated names. The metadata stored in the trie only contains the
 * opening name associated with its key; other statistics are left to default
 * values.
 * This is the 'initial' trie. *)
let init_eco f =
  let eco_list = parse_eco f in
  ()

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f =
  let replays = load_png f in
  ()


let opening_name o = failwith "opening_name: Unimplemented"
