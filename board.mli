(* Represents the color of the piece. *)
type color = | White | Black

(* Represents the type of the piece *)
type piece_rank =
  | King of bool
  | Queen
  | Rook of bool
  | Knight
  | Bishop
  | Pawn of bool * bool

(* Represents a piece in chess. *)
type piece = color * piece_rank

(* Represents a square on a chessboard as a tuple, where the first int is the
 * file and the second int represents the rank. Both numbers should be between
 * 1 and 8, inclusive. *)
type position = int * int

type move = position * position

type last_move = (piece * move) option

(* Represents the movement of a piece from one position to another as a tuple,
 * where the first position is the initial position and the second position is
 * the final position. *)
(* type move *)

type move_list

type move_history

(* Represents the chessboard as a list of squares on the board and the pieces
 * that occupy them. The first entry in the tuple represents white's pieces
 * while the second represents black's pieces. Any position not listed is
 * assumed to be an empty space. *)
type board

(* Represents a starting board, set up according to chess convention. *)
val init_board : board

val print_board : board -> string

type check

(* Represents the end-game state. A game can either end in a checkmate or
 * stalemate when a player can no longer make any legal moves *)
type end_game

val get_piece : board -> position -> piece option

val oppc : color -> color

val piece_string: piece_rank -> color -> string

(* [legal_moves b c] returns a list of legal moves by player [c] given
 * board [b]. *)
val legal_moves : board -> last_move -> color -> (move * board) list

(* [update_board b m] takes a board [b] and returns an updated board after
 * performing move [m]. If [m] is not a legal move, return the same board. *)
val make_move : board -> color -> last_move -> move -> (move * board) list-> (board * check)


(* The functions below convert moves into "algebraic chess notation"
 * Details of how they are formatted found here:
 *   https://en.wikipedia.org/wiki/Portable_Game_Notation#Movetext
 * *)
(* [to_algno lm b m]
 * Converts [m] to algebraic notation given that [b] is the state of the board
 * PRIOR move [m], and [lm] was the last move made. *)
val to_algno : last_move -> board -> move -> string

(* [from_algno lm b s]
 * Reverses to_algno.
 * Requires : [s] is a validly formed and legal given board [b]. fails otherwise *)
val from_algno : last_move -> board -> string -> move
