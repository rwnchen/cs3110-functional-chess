(* Represents the color of the piece. *)
type color = | White | Black

(* Represents the type of the piece *)
type piece_rank

(* Represents a piece in chess. *)
type piece

(* Represents a square on a chessboard as a tuple, where the first int is the
 * file and the second int represents the rank. Both numbers should be between
 * 1 and 8, inclusive. *)
type position = int * int

type move = position * position

(* Represents the movement of a piece from one position to another as a tuple,
 * where the first position is the initial position and the second position is
 * the final position. *)
(* type move *)

type move_list

type move_history

val mh: move_history

(* Represents the chessboard as a list of squares on the board and the pieces
 * that occupy them. The first entry in the tuple represents white's pieces
 * while the second represents black's pieces. Any position not listed is
 * assumed to be an empty space. *)
type board

(* Represents a starting board, set up according to chess convention. *)
val init_board : board

type check

(* Represents the end-game state. A game can either end in a checkmate or
 * stalemate when a player can no longer make any legal moves *)
type end_game

val get_piece : board -> position -> piece option

(* [legal_moves b c] returns a list of legal moves by player [c] given
 * board [b]. *)
val legal_moves : board -> move_history -> color -> (move * board) list

(* [update_board b m] takes a board [b] and returns an updated board after
 * performing move [m]. If [m] is not a legal move, return the same board. *)
val make_move : board -> color -> move -> (move * board) list-> (board * check)
