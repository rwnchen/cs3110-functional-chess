(* Metadata associated with each opening sequence, such as white/black
 * winrates, opening name, etc. *)
type opmetadata

(* Represents the database of stored openings
 *
 * We will have to find some way of saving this database beforehand and loading
 * it every time the application begins. *)
type openings

(* [init_openings s]
 * Given the path to the openings database on disk [s], loads and returns the
 * database *)
val init_openings : string -> openings

(* [opening_name d o]
 * Returns the name of the opening move sequence represented by [o] in
 * the openings database [d]
 * an example [o]: ["g3"; "e5"; "Nf3"] *)
val opening_name : openings -> string list -> string

(* [white_winrate d o]
 * Returns WHITE's winrate of the opening move sequence represented by [o]
 * in the openings database [d]
 * an example [o]: ["g3"; "e5"; "Nf3"] *)
val white_winrate : openings -> string list -> float


(* [best_reply d o n]
 * Returns up to [n] "best" replies for WHITE if [o] is EVEN length, or BLACK if [o]
 * has an ODD length ([o] should be the move history up to this point)
 *   d - openings
 *   o - the current sequence of moves already made, in algebraic notation ([] if no moves made)
 *   n - the maximum number of best replies to give. May be fewer or even [] if no more replies
 *       are available in [d]. "best" is defined by having the highest winrate
 * The replies are 1 move and in standard algebraic notation. *)
val best_reply : openings -> string list -> int -> string list
