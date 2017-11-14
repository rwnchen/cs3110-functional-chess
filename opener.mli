(* open game engine types *)
open Trie

(* TODO: Remove this once the game/board engine types have been defined *)
(* Represents the current state of the game board
 *
 * What's needed: The current sequence of moves of the game *)
type board

(* Metadata associated with each opening sequence, such as white/black
 * winrates, move name, etc. *)
type metadata

(* Represents an actual opening move SEQUENCE
 * TODO: replace string with the representation for moves. *)
type opmoves = metadata * string

(* Represents the database of stored openings
 *
 * We will have to find some way of saving this database beforehand and loading
 * it every time the application begins. *)
type openings

(* [init_openings s]
 * Given the path to the openings database on disk [s], loads and returns the
 * database. *)
val init_openings : string -> openings

(* [suggest_moveset o b]
 * Given the database of openings [o] and the current board state [b],
 * suggest a set (possibly empty) of possible next moves from ECO. *)
val suggest_moveset : openings -> board -> opmoves list

(* [suggest_move o b]
 * Similar to [suggest_moveset o b], but simply returns the opening sequence
 * with the highest win rate for the player whose turn it currently is. *)
val suggest_move : openings -> board -> opmoves option

(* [opening_name o]
 * Returns the name of the opening move sequence represented by [o] *)
val opening_name : opmoves -> string

