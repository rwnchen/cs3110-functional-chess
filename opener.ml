open Str
open Board
open Trie
open Yojson.Basic.Util
open String
(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Representation types *)

(* Trie database *)
module StrMap = Map.Make(String)
module StrTrie = Make(StrMap)

(* Metadata associated with each opening, such as white/black winrates etc.
 *)
type opmetadata =
  {
    mutable total_count : int; (* Number of times this op appeared in ECO games *)
    mutable white_wins : int;  (* Number of times white won when this op appeared *)
    name : string;             (* ECO name *)
    category : string;         (* ECO category *)
  }

type openings = opmetadata StrTrie.t

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Helper functions/states *)


(* Empty openings database *)
let initial_op_trie = StrTrie.empty

(* Initial metadata for a moveset, with name and ECO category only *)
let init_meta n c = {
  total_count = 0;
  white_wins = 0;
  name = n;
  category = c;
}

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

(* [parse_eco f]
 * string -> string list list
 * Given the path of a json file [f], parses [f] into a list of ECO chess
 * openings. Each opening in the list is a string list, with at least 3
 * elements. The first element is the ECO categor(ies) of the sequence, e.g.
 * "A43". The second element is the name of the opening, and the following
 * elements are the moves associated with that opening.
 *
 * e.g. ["A42", "King's Random Opening", "e5 f3", "c5, a1"] *)
let parse_eco f =
  let js = Yojson.Basic.from_channel (open_in f) in
  let eco_list_js = js |> member "openings" |> to_list in
  let rec to_eco_list l r =
    match l with
    | [] -> r
    | h::t -> to_eco_list t ((h |> to_list |> json_to_string_list)::r)
  in
  let eco = to_eco_list eco_list_js [] in
  let split_op e =
    match e with
    | category::name::moves -> (category, name, moves)
    | _ -> failwith "split_op: Invalid opening supplied!"
  in
  List.map split_op eco
  (*let eco = to_eco_list eco_list_js [] in
  List.iter (fun l -> List.iter (fun x -> Printf.printf "%s\t" x) l) eco'*)

(* [split_op e]
 * splits *)
let split_op e =
  match e with
  | category::name::moves -> (category, name, moves)
  | _ -> failwith "split_op: Invalid opening supplied!"

(* [init_eco f]
 * string -> openings
 * Constructs the openings trie given a json file [f] with all openings and
 * their associated names. The metadata stored in the trie only contains the
 * opening name associated with its key; other statistics are left to default
 * values.
 * This is the 'initial' trie. *)
let init_eco f =
  let eco_list = parse_eco f in
  let rec add_all l trie =
    match l with
    | [] -> trie
    | h::t ->
      begin
        match h with
        | (category, name, moves_list) ->
          (* initial metadata for this opening sequence, just name and category *)
          let opmeta = init_meta name category in
          add_all t (StrTrie.add moves_list opmeta trie)
        | _ -> failwith "init_eco: Invalid opening supplied!"
      end
  in
  add_all eco_list initial_op_trie

(* [construct_openings]
 * This is a one-off function that constructs the initial trie with all ECO openings
 * and names, but NO statistics (white wins, # of occurences, etc.). It then reads the
 * FICS replay file (specified in the directory above), and uses those replays to BUILD UP
 * the statistics associated with different openings.
 * How it does this:
 *   for each match in the replay file, it looks at the sequence of moves made by the players,
 *   and considers progressively longer prefixes of the move sequence. Each prefix is treated
 *   as a key into the trie, and as long as the prefix is still a valid key, it will update
 *   the metadata stored into the trie at that prefix key. For example, if the move sequence
 *   consisted of: "e4 e5 Nf3 g3 a1 c5 ...", and there existed keys "e4", "e4 e5", "e4 e5 Nf3"
 *   then three opmetadata in the trie will be updated to reflect the results of the matchs.
 *   Since the prefix "e4 e5 Nf3 g3" is not in the trie, we move onto the next game.
 * Once the entire replay file has been read, the trie is then stored back onto disk as a
 * json file. *)
let construct_openings =
  let op_trie = init_eco eco_opening_json in
  ()

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f =
  init_eco eco_opening_json (* replace later *)


let opening_name o = failwith "opening_name: Unimplemented"
