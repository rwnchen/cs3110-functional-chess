open Piece

type position = int * int

type move = position * position

type board = (position * piece) list * (position * piece) list

type init_board = board

type end_game = | Checkmate | Stalemate

let is_check b m = failwith "unimplemented"

(* [moves p pos] returns a list positions that piece [p] can move to, given that
 * it is in position [pos] on the board. *)
let moves p pos = failwith "unimplemented"

let legal_moves b c = failwith "unimplemented"

let end_type b = failwith "unimplemented"

let update_board b m = failwith "unimplemented"
