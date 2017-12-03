open Str
open Printf

(* EXAMPLE USAGE *)
(* Assuming that "replays/test_pgn.pgn" exists (which it should)
   Loading the test PGN files:
     let replays = load_pgn "replays/test_pgn.pgn"

   Saving a replay into "example.pgn" from a list of replays:
     replays |> List.hd |> save_pgn "replays/example.pgn"
   or
     let a_replay = List.hd replays in
     save_pgn "example.pgn" a_replay
 *)

(* Redeclared because ocaml. *)
type tag_pair =
  | Event of string
  | Site of string  (* Location in which the game was held *)
  | Date of string  (* Formatting of date may vary depending on pgn *)
  | Round of string (* May not be an int (e.g. unfinished, 'N/A') *)
  | White of string (* Name of white player *)
  | Black of string (* Name of black player *)
  | Result of string         (* "1-0" (white won), "0-1" (black won), or "1/2-1/2" (draw) *)
  | Tag of string * string   (* tag name * tag contents *)

type replay =
  {
    mutable tags : tag_pair list;  (* The tag pairs *)
    mutable moves : string array;  (* The set of moves *)
  }

let tag_rx = regexp "\\[\\|\\]"
let quote_trim = regexp {|"|}
let move_rx = regexp {| *[0-9]+\. \| +|}
let delim = regexp {| "|}
let comment_rx = regexp {|\{.*\}|}
let result_rx = regexp {|0-1\|1-0\|1/2-1/2|}

(* Helper functions *)
(* [empty_replay ()]
 * Thunk to construct a new empty replay. *)
let empty_replay () = { tags = []; moves = [||]; }

(* [build_tag h t]
 * Constructs a tag_pair given the tag name as [h] and the tag content
 * as [t] *)
let build_tag h t =
  match h with
  | "Event" -> Event(t)
  | "Site" -> Site(t)
  | "Date" -> Date(t)
  | "Round" -> Round(t)
  | "White" -> White(t)
  | "Black" -> Black(t)
  | "Result" -> Result(t)
  | _ -> Tag(h, t)

(* [parse_tag line tags]
 * Constructs a tag_pair from [line] and returns after prepending it onto [tags]
 * If [line] does NOT contain a valid tag pair, simply returns [tags]*)
let parse_tag line tags =
  let line' = global_replace tag_rx "" line in (* Trims out the [] from the line *)
  let split = bounded_split delim line' 2 in   (* Splits tag name and tag content *)
  match split with
  | h::t::[] ->
    let s = global_replace quote_trim "" t in  (* Trims out extra quotations *)
    (build_tag h s)::tags
  | _ ->
    tags

let tag_to_string = function
  | Event(t) -> sprintf {|Event "%s"|} t
  | Site(t) -> sprintf {|Site "%s"|} t
  | Date(t) -> sprintf {|Date "%s"|} t
  | Round(t) -> sprintf {|Round "%s"|} t
  | White(t) -> sprintf {|White "%s"|} t
  | Black(t) -> sprintf {|Black "%s"|} t
  | Result(t) -> sprintf {|Result "%s"|} t
  | Tag(h, t) -> sprintf {|%s "%s"|} h t

(* [parse_moves line]
 * Returns an array of all the white-black move pairs in the line [line]
 *
 * All comments {comments} disappear
 * The game results (e.g. 1-0 0-1 1/2-1/2) also are NOT included *)
let parse_moves line =
  line |>
  global_replace comment_rx "" |> (* delete comments *)
  global_replace result_rx "" |>  (* delete result *)
  split move_rx |>
  Array.of_list

(* [read_replay c]
 * Tries to read exactly one replay given an input channel for a
 * pgn file [c].
 * Closes the channel on EOF
 *
 * Will use this to construct a replay-stream reader, maybe. *)
let read_replay in_channel =
  let r = empty_replay () in
  let read_replay = ref false in
  try
    while not !read_replay do
      let line = in_channel |> input_line |> String.trim  in

      (* Simple parsing *)
      if string_match tag_rx line 0 then
        begin
          r.tags <- parse_tag line r.tags
        end
      else if string_match move_rx line 0 then
        begin
          r.moves <- parse_moves line;
          read_replay := true
        end
      else
        ()
    done;
    Some r
  with
    End_of_file -> close_in in_channel; None

(* Exposed module functions  *)
let load_pgn file =
  let rec load_pgn' pgn in_channel =
    match read_replay in_channel with
    | None -> pgn
    | Some(r) -> load_pgn' (r::pgn) in_channel
  in
  file |> open_in |> load_pgn' []

let get_move r n =
  try Some r.moves.(n) with
  | _ -> None

let moves_list r = Array.to_list r.moves

let tags r = r.tags

let to_replay m t = {tags = t; moves = Array.of_list m}

let save_pgn f r =
  let rec save_tags out tags =
    match tags with
    | h::t ->
      h |> tag_to_string |> fprintf out "[%s]\n";
      save_tags out t
    | [] -> ()
  in
  let out_channel = open_out f in

  (* Save the replay tags *)
  save_tags out_channel r.tags;
  fprintf out_channel "\n";

  (* Save the move history *)
  for i = 0 to Array.length r.moves - 1 do
    fprintf out_channel "%d. %s" (i+1) r.moves.(i)
  done;

  (* Lastly, save the game result as 1-0 0-1 or 1/2-1/2 *)
  let rec result tags =
    match tags with
    | Result(r)::t -> r
    | _::t -> result t
    | _ -> failwith "save_pgn: No result in tags!"
  in
  fprintf out_channel "%s" (result r.tags);

  (* Newlines and close channel *)
  fprintf out_channel "\n\n";
  close_out out_channel
