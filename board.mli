open Piece

(* Represents a square on a chessboard as a tuple, where the first int is the
 * file and the second int represents the rank. Both numbers should be between
 * 1 and 8, inclusive. *)
type position

(* Represents the movement of a piece from one position to another as a tuple,
 * where the first position is the initial position and the second position is
 * the final position. *)
type move

(* Represents the chessboard as a list of squares on the board and the pieces
 * that occupy them. The first entry in the tuple represents white's pieces
 * while the second represents black's pieces. Any position not listed is
 * assumed to be an empty space. *)
type board

(* Represents a starting board, set up according to chess convention. *)
type init_board

(* Represents the end-game state. A game can either end in a checkmate or
 * stalemate when a player can no longer make any legal moves *)
type end_game

(* [is_check b m] returns whether or not a board [b] would be in check as a
 * result of the given move [m]. *)
val is_check : board -> move -> bool

(* [legal_moves b c] returns a list of legal moves by player [c] given
 * board [b]. *)
val legal_moves : board -> color -> move list

(* [end_type b] returns whether the game ends in a checkmate or a stalemate. To
 * be called when there are no legal moves found. *)
val end_type : board -> end_game

(* [update_board b m] takes a board [b] and returns an updated board after
 * performing move [m]. If [m] is not a legal move, return the same board. *)
val update_board : board -> move -> board
