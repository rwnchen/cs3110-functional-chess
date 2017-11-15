(* [move_piece from to] is true if moving the piece from [from] to [to] is
   a sucess, and false otherwise.
*)
val move_piece: string -> string -> bool

(* [highlight tile_list] sends [tile_list] as a list of tiles to be highlighted
   in the gui. Is true iff the operation was successful and false otherwise
*)
val highlight: string list -> bool

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

(* [click_listener ()] checks if there was a click on the gui recently.
   It is None if there is no click, and it is Some string representing the tile
   or button that was clicked on.
*)
val click_listener: unit -> string option
