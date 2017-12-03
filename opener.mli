(* Metadata associated with each opening sequence, such as white/black
 * winrates, opening name, etc. *)
type opmetadata

(* Represents an actual opening move SEQUENCE
 * TODO: replace string with the representation for moves. *)
(* type opmoves *)

(* Represents the database of stored openings
 *
 * We will have to find some way of saving this database beforehand and loading
 * it every time the application begins. *)
type openings

(* [init_openings s]
 * Given the path to the openings database on disk [s], loads and returns the
 * database. *)
val init_openings : string -> openings

(* [opening_name o]
 * Returns the name of the opening move sequence represented by [o] *)
(* val opening_name : opmoves -> string *)
