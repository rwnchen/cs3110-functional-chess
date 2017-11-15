(* Represents the color of the piece. *)
type color = | White | Black

(* Represents the type of the piece *)
type piece_rank =
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

(* Represents a piece in chess. *)
type piece = color * piece_rank
