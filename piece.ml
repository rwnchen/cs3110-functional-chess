type color = | White | Black

type piece_rank =
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

type piece = color * piece_rank