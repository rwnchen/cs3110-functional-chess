open Str

(* Redeclared because ocaml. *)
type tag_pair =
  | Event of string
  | Site of string  (* Location in which the game was held *)
  | Date of string  (* Formatting of date may vary depending on pgn *)
  | Round of string (* May not be an int (e.g. unfinished, 'N/A') *)
  | White of string (* Name of white player *)
  | Black of string (* Name of black player *)
  | Result of string         (* TODO: Consider a new type for game state*)
  | Tag of string * string   (* tag name * tag contents *)

type replay =
  {
    mutable tags : tag_pair list;  (* The tag pairs *)
    mutable moves : string array;  (* The set of moves *)
  }

let empty_replay = { tags = [];
                     moves = [||]; }

let tag_trim = Str.regexp "\[\|\]"
let quote_trim = Str.regexp {|"|}
let move_regexp = Str.regexp " [0-9+]. "
let delim = Str.regexp {| "|}

(* Helper functions *)
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
 * Parses a tag *)
let parse_tag line tags =
  let line' = Str.global_replace tag_trim "" line in (* Trims out the [] from the line *)
  (*Printf.printf "Trimmed: %s\n" line';*)
  let split = Str.bounded_split delim line' 2 in     (* Splits tag name and tag content *)
  match split with
  | h::t::[] ->
    let s = Str.global_replace quote_trim "" t in    (* Trims out extra quotations *)
    (*Printf.printf "Head: %s | Tail: %s\n" h s;*)
    (build_tag h s)::tags
  | _ ->
    (*print_endline "No split match";*)
    tags

(* [parse_moves line]
 * Returns an array of all the white-black move pairs in the line [line] *)
let parse_moves line =
  line |> Str.split move_regexp |> Array.of_list

(* [read_replay f]
 * Tries to read exactly one replay from the given file [f].
 * Will use this to construct a replay-stream reader, maybe. *)
let read_replay file =
  let in_channel = open_in file in
  let r = empty_replay in
  let read_replay = ref false in
  try
    while not !read_replay do
      let line = in_channel |> input_line |> String.trim  in

      (* Simple parsing *)
      if String.get line 0 == '[' then
        begin
          Printf.printf "Tag line %s\n" line;
          r.tags <- parse_tag line r.tags
        end
      else if String.get line 0 == '1' then
        begin
          Printf.printf "Moves line %s\n" line;
          r.moves <- parse_moves line;
          read_replay := true
        end
      else
        Printf.printf "Non line %s\n" line;
    done;
    Some r
  with
    End_of_file -> close_in in_channel; Some r

(* Exposed module functions
 * TODO EVERYTHING *)
let load_pgn file =
  let in_channel = open_in file in
  let replays = [] in
  let x = ref 0 in
  let readfile =
  try
    while !x < 4 do
      let line = input_line in_channel in
      Printf.printf "line is: %s\n" line;
      x := !x+1;
    done
  with End_of_file -> close_in in_channel in
  readfile;
  replays

let get_move r n =
  try Some r.moves.(n) with
  | _ -> None

let moves_list r = Array.to_list r.moves

let tags r = r.tags

let save_pgn r f = ()
