(* There are seven tag pairs common to every PGN file, which are:
 *   - Event: the name of the tournament or match event.
 *   - Site: the location of the event. This is in City, Region COUNTRY format, where COUNTRY is the three-letter International Olympic Committee code for the country. An example is New York City, NY USA.
 *   - Date: the starting date of the game, in YYYY.MM.DD form. ?? is used for unknown values.
 *   - Round: the playing round ordinal of the game within the event.
 *   - White: the player of the white pieces, in Lastname, Firstname format.
 *   - Black: the player of the black pieces, same format as White.
 *   - Result: the result of the game. This can only have four possible values: 1-0 (White won), 0-1 (Black won), 1/2-1/2 (Draw), or * (other, e.g., the game is ongoing).
 *
 * Additionally, there may be an arbitrary number of additional tags.
 * For more information, see the Wikipedia page for Portable Game Format.  *)
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

(* Represents a single replayed game in the format used by the PGN
 * replay module. The replay contains more information than simply the
 * state of the game board.
 * replay is immutable. *)
type replay

(* [load_pgn file]
 * Loads and returns the .pgn file specified by [file]
 *
 * Returns the list of all replays stored in [file]. Note that a single pgn file
 * may store the contents of *multiple* chess games. The returned list will contain
 * each game in the order that they appear in the pgn file. *)
val load_pgn : string -> replay list

(* [get_move r n]
 * Obtains a string representing white and black's [n]th moves for replay [r]
 * Returns None if [n] is out of bounds or [r] is invalid *)
val get_move : replay -> int -> string option

(* [moves_list r]
 * Returns the list of moves associated with replay [r] *)
val moves_list : replay -> string list

(* [tags r]
 * Returns a list of all the tag_pairs associated with the replay. *)
val tags : replay -> tag_pair list

(* [save_pgn r file]
 * Saves the replay [r] to the pgn file specified by [file]. If [file]
 * does not exist, a new pgn file will be created. If [file] already
 * contains one or more saved replays, [r] will be *appended* to the end
 * of [file]. *)
val save_pgn : replay -> string -> unit

(* TODO: What other kinds of data is needed from a chess replay game? *)
