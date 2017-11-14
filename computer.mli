(* A possible extension
 * Proposed interface file for implementing an AI for playing chess
 * *)

(* Represents the current state of the game *)
type game

(* [play_move g]
 * Given a game state [g], returns the next move for black/white *)
val play_move : game -> string
