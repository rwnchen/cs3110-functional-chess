open Board
open Replayer
(* open Controller *)
open Opener

(* Represents any metadata associated with the current chess game
 * See replayer.mli for a list of all non-optional tags that must
 * be present.
 * *)
(* type metadata *)

(* Represents the state of a chess game, including board and
 * metadata information, as well as whose turn it currently is.
 *
 * This type has to be accessed by multiple different modules
 * EDIT: Not anymore! The application module is strictly superior to
 * the others; no other module will require any of the types defined
 * here. Functionality that did has been moved to this module instead. *)
(* type game *)

(* Represents the state of the application overall.
 * Data associated with the state of the application include:
 *   - the state of the current chess game, if any
 *   - the state of the current replay, if any
 *   - metadata associated with the current game/replay, such as
 *     who's playing, time played, etc.
 * Which are represented by the [game] and [metadata] types, but
 * additionally, we may also have:
 *   - the chat log
 *   - the opening database currently loaded
 * *)
type state

(* Provide chess openings *)
(* [suggest_moveset o g]
 * Given the database of openings [o] and the current game state [g],
 * suggest a set (possibly empty) of possible next moves from ECO. *)
(* val suggest_moveset : openings -> game -> opmoves list *)

(* [suggest_move o g]
 * Similar to [suggest_moveset o g], but simply returns the opening sequence
 * with the highest win rate for the player whose turn it currently is. *)
(* val suggest_move : openings -> game -> opmoves option *)

(* Replays *)
(* [to_replay g]
 * Converts a game [g] to a replay format ready for writing. *)
(* val to_replay : game -> replay *)

(* [initial_state] is the state the application starts in.
 * Presumably, this should be the same for all instances of the app,
 * unless we want to save/load things from cache somewhere. *)
val initial_state : int

(* Runs the application, given some initial state.
 * I imagine this will behave similar to a repl, but without the printing
 * to command line. *)
val run : state -> unit
