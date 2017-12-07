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

(* Represents a move in chess as a tuple of positions. The first of the tuple is
 * the initial position, while the second represents the final position. *)
type move = position * position

(* Represents the last move made on a board. Is None at the start of the game,
 * otherwise should contain a tuple of the piece that made the move and the move
 * itself. *)
type last_move = (piece * move) option

(* Represents the chessboard as a list of squares on the board and the pieces
 * that occupy them. The first entry in the tuple represents white's pieces
 * while the second represents black's pieces. Any position not listed is
 * assumed to be an empty space. *)
type board = (position * piece) list * (position * piece) list

(* A type to describe the state of check of a board: whether or not its in check
 * as well as which color is in check. *)
type check = | Black_Check | White_Check | No_Check

(* [init_board] is a board with all pieces in their initial positions. Returns a
 * tuple of lists of pieces and their positions. The first list is the black
* pieces, while the second is the white. *)
val init_board : board

(* [print_board b] prints an ASCII representation of board [b].  *)
val print_board : board -> string

(* [get_piece b pos] retrieves a piece at a given position on a board. *)
val get_piece : board -> position -> piece option

(* [oppc c] returns the opposite color of [c]. *)
val oppc : color -> color

(* [piece_string p c] converts pieces to strings of their unicode counterparts.
 * Taken from https://github.com/shrumo/chess-engine *)
val piece_string: piece_rank -> color -> string

(* [legal_moves b last_move c] returns a list of all legal moves by a given
 * color. A legal move does not put one's own king in check. *)
val legal_moves : board -> last_move -> color -> (move * board) list

(* [make_move b c last_move m leg_mves] Updates a board with a legal move. Returns the new board, as well as whether
 * or not this move puts the opponent in check. If the move is illegal, returns
 * the same board. *)
val make_move : board -> color -> last_move -> move -> (move * board) list-> (board * check)

(* [promote b c last_move file newp]
 * Updates a board for promotion by replacing the promoting pawn in the piece
 * list with either a queen, rook, bishop, or knight. Returns a new board with
 * the promoted piece and whether or not the new board is in check. *)
val promote : board -> color -> last_move -> int -> string -> (board * check)

(* The functions below convert moves into "algebraic chess notation"
 * Details of how they are formatted found here:
 *   https://en.wikipedia.org/wiki/Portable_Game_Notation#Movetext
 * *)
(* [to_algno p lm b m ]
 * Converts [m] to algebraic notation given that [b] is the state of the board
 * PRIOR move [m], and [lm] was the last move made.
 *
 * [p] should be set to Some (piece_rank) if [m] is a move that promotes a pawn
 * to (piece_rank). [p] can be left out otherwise. *)
val to_algno : ?promote:piece_rank option -> last_move -> board -> move -> string

(* [from_algno pr_target lm b s]
 * Reverses to_algno.
 *
 * After calling this function, [pr_target] will contain the target piece_rank of a
 * promotion move, or None if [s] is not promoting.
 * Requires : [s] is a validly formed and legal given board [b]. fails otherwise *)
val from_algno : piece_rank option ref -> last_move -> board -> string -> move
