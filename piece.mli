open Board

(* Represents the color of the piece. *)
type color = | White | Black

(* Represents the type of the piece *)
type piece =
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

(* Represents a piece in chess. *)
type piece = color * piece_type

(* [moves p pos] returns a list positions that piece [p] can move to, given that
 * it is in position [pos] on the board. *)
val moves : piece -> position -> position list