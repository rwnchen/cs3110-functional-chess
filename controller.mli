(* gui_state is the state of the gui represented as a python object*)
type gui_state = Lymp.pyobj

(* type position = Board.position
type move = Board.move *)
type position = int * int

type move = position * position

(* These are basic values that are needed to connect to a python file *)
val interpreter: string
val py: Lymp.pycommunication
val gui: Lymp.pycallable

(* [move_piece state move] performs the [move] in the gui [state]
*)
val move_piece: Lymp.pyobj ref -> move -> Lymp.pyobj

(* [highlight tile_list] sends [tile_list] as a list of positions to be
   highlighted in the gui.
*)
val highlight: Lymp.pyobj ref -> position list -> Lymp.pyobj

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
val update_history: Lymp.pyobj ref -> (string*Board.board) list  -> Lymp.pyobj
