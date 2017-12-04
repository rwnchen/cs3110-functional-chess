open Str
open Board
open Trie
open Yojson.Basic.Util
open String
open Replayer
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
let initial_op_trie () = StrTrie.empty

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
let eco_opening_data = dir ^ "eco_openings"

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
  add_all eco_list (initial_op_trie ())

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* ***** ***** ***** * Json Parsing (trie or metadata) * ***** ***** ***** *)
(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* [meta_to_json m]
 * Converts opmetadata [m] into a basic Yojson association list. *)
let meta_to_json m =
  `Assoc [ ("total_count", `Int m.total_count);
           ("white_wins", `Int m.white_wins);
           ("name", `String m.name);
           ("category", `String m.category); ]

(* [json_to_meta j]
 * Does the reverse of [meta_to_json] above, returning the metadata from json assoc list *)
let json_to_meta j =
  let tc = j |> member "total_count" |> to_int in
  let ww = j |> member "white_wins" |> to_int in
  let n = j |> member "name" |> to_string in
  let c = j |> member "category" |> to_string in
  let meta = init_meta n c in
  meta.total_count <- tc;
  meta.white_wins <- ww;
  meta

(* [move_to_json m]
 * Converts a list of moves e.g. ["e5"; "c5"; "Nf3"] into a json list of strings
 * Order is preserved. *)
let move_to_json m =
  `List (List.map (fun move -> `String move) m)

(* [json_to_move j]
 * Reverses the above. See notes above. *)
let json_to_move j =
  convert_each (fun move -> move |> to_string) j

(* [open_to_json t]
 * Converts the entire opening database trie [t] into a json object (NO side effects) *)
let trie_to_json op_trie =
  let json_assoc = ref [] in
  StrTrie.iter
    (fun k v ->
       let json =
         `Assoc[ ("metadata", meta_to_json v);
                 ("moves_list", move_to_json k); ]
       in
       json_assoc := json::(!json_assoc);) (* Imperative rules functional drools *)
    op_trie;
  `List (!json_assoc)

(* [json_to_open j]
 * Reverses the above, reading the json object and returning a new [openings] trie *)
let json_to_trie j =
  let trie = ref (initial_op_trie ()) in
  List.iter
    (fun move ->
       let metadata = move |> member "metadata" |> json_to_meta in
       let moves_list = move |> member "moves_list" |> json_to_move in
       trie := StrTrie.add moves_list metadata !trie; ())  (* Imperative rules functional drools *)
    (to_list j);
  !trie

(* [tag_result t]
 * Returns the string in Result(...) in the tag set [t].
 * NOTE: The invariant for tag_pairs say that all tags must have a result! *)
let rec tag_result tags =
  match tags with
  | Result(r)::t -> r
  | h::t -> tag_result t
  | [] -> failwith "tag_result: invalid tag format!"

(* [update_metadata m t]
 * Updates (MUTATES) the metadata [m] given a tags_pair list [t]
 *
 * Imperative ru-- *)
let update_metadata meta tags =
  let w_won = "1-0" = (tags |> tag_result) in
  meta.total_count <- meta.total_count + 1;
  meta.white_wins <- meta.white_wins + (if w_won then 1 else 0)

(* Helper function to [construct_openings] *)
(* [prefix i lst]
 * Returns the first [i] elements of [lst] (the prefix of [lst]) in order head->tail
 * Requires 0 <= i <= List.length lst. Throws an exception otherwise *)
let prefix i lst =
  let rec prefix' i lst ret =
    if i = 0 then List.rev ret
    else
      match lst with
      | h::t -> prefix' (i-1) t (h::ret)
      | [] -> failwith "prefix: Invalid i"
  in
  prefix' i lst []

(* [construct_openings ()]
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
 * json file. Oh my god OCaml yojson is a real pain in the neck... Should've used ATDgen *)
let construct_openings () =
  let op_trie = init_eco eco_opening_json in

  (* Read the pgn file here *)
  let replays = load_pgn ficsgames_1 in
  let rec build_stats op_trie replay_list =
    match replay_list with
    | r::t ->
      begin
        (* Examine replay [r] and update all metadata in [op_trie] as necessary *)
        let moves = moves_list r in
        let tag_pairs = tags r in
        let prefix_in = ref true in
        let i = ref 1 in
        while (!prefix_in) do
          if List.length moves < !i then
            prefix_in := false
          else
            let prefix_i = prefix (!i) moves in         (* prefix of moves *)
            if StrTrie.mem prefix_i op_trie then
              begin
                let meta = StrTrie.find prefix_i op_trie in
                update_metadata meta tag_pairs          (* mutates [meta] *)
              end
            else
              prefix_in := false;
            i := !i + 1
        done;

        (* recurse on the remaining replays *)
        build_stats op_trie t
      end
    | [] -> ()
(*
          for i = 1 to List.length (moves) do
            let prefix = i |> Array.sub moves_arr 0 |> Array.to_list in
            Printf.printf "Prefix head: %s\t" (List.hd prefix);
            if StrTrie.mem prefix op_trie then
              begin
                update_metadata prefix op_trie (white_won h);
              end
            else
              raise breakloop;
          done;
        with _ ->
          build_stats op_trie t
      end
*)
  in
  build_stats op_trie replays;

  (* Finally, converts the trie to a json file and saves it to [eco_opening_data] *)
  let oc = open_out_gen [Open_append; Open_trunc; Open_creat] 0 eco_opening_data in
  let trie_json = trie_to_json op_trie in
  Yojson.Basic.to_channel oc trie_json;
  close_out oc


(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Debugging functions *)
let strip str =
  let str = replace_first (regexp "^ +") "" str in
  replace_first (regexp "; +$") "" str

let list_to_string lst =
  let output_str = ref "[" in
  List.iter (fun i -> output_str := !output_str ^ i ^ "; ") lst;
  (strip (!output_str)) ^ "]"

let meta_to_string meta =
  "{" ^ "total_count: " ^ string_of_int meta.total_count
      ^ "\twhite_wins: " ^ string_of_int meta.white_wins
      ^ "\tname: " ^ meta.name
      ^ "\tcategory: " ^ meta.category
      ^ "}"

let print_keys op_trie =
  StrTrie.iter (fun k v -> Printf.printf "*********************\nKey: %s\nValue: %s\n" (list_to_string k) (meta_to_string v)) op_trie

(* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *)
(* Exposed functions *)

let init_openings f =
  let in_channel = open_in eco_opening_data in
  let trie_json = Yojson.Basic.from_channel in_channel in
  json_to_trie trie_json

let opening_name trie moves = (StrTrie.find moves trie).name

let white_winrate trie moves =
  let metadata = StrTrie.find moves trie in
  try
    (metadata.white_wins |> float_of_int) /. (metadata.total_count |> float_of_int)
  with _ -> 0.0
