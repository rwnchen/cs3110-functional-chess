(* open lymp *)

(* [move_piece (x1,y1) (x2,y2)] is the new gui state after trying to move the
   piece at (x1,y1) to (x2,y2)
*)
(* val move_piece: (int*int) -> (int*int) -> pyobj *)

(* [highlight tile_list] sends [tile_list] as a list of tiles to be highlighted
   in the gui. Is true iff the operation was successful and false otherwise
*)
(* val highlight: string list -> bool *)

(* [openers opener_list] sends [opener_list] to the gui to display. Is true iff
   the operation was successful and false otherwise
   TODO: will probably want to change to something like "opener list" or
   something once we get that far
*)
val openers: string list -> bool

(* [history move_list] sends [move_list] to the gui to display. Is true iff
   the operation was successful and false otherwise
   TODO: will probably want to change to something like "move list" or
   something once we get that far.
*)
val history: string list -> bool
