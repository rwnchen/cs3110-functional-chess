(* Metadata associated with each opening sequence, such as white/black
 * winrates, opening name, etc. *)
type opmetadata

(* Represents the database of stored openings
 *
 * We will have to find some way of saving this database beforehand and loading
 * it every time the application begins. *)
type openings

(* [init_openings ()]
 * loads and returns the openings database from the standard location *)
val init_openings : unit -> openings

(* [opening_meta d seq]
 * Returns the [opmetadata] associated with the move sequence [seq] in the
 * database [d]
 *
 * If [seq] is not in [d], raises Not_found *)
val opening_meta : openings -> string list -> opmetadata

(* [opening_name o]
 * Returns the name of the opening move sequence represented by [o] *)
val opening_name : opmetadata -> string

(* [eco_category o]
 * Returns the ECO category of the opening move sequence represented by [o] *)
val eco_category : opmetadata -> string

(* [white_winrate o]
 * Returns WHITE's winrate of the opening move sequence represented by [o]
 * If [o] has never been seen before, returns 0.0 (welp) *)
val white_winrate : opmetadata -> float


(* [best_reply d o n]
 * Returns up to [n] "best" replies for WHITE if [o] is EVEN length, or BLACK if [o]
 * has an ODD length ([o] should be the move history up to this point)
 *   d - openings
 *   o - the current sequence of moves already made, in algebraic notation ([] if no moves made)
 *   n - the maximum number of best replies to give. May be fewer or even [] if no more replies
 *       are available in [d]. "best" is defined by having the highest winrate
 * The replies are 1 move and in standard algebraic notation. *)
val best_reply : openings -> string list -> int -> string list
