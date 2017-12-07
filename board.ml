open Str
(******************************************************************************)
(***************************** TYPE DECLARATIONS ******************************)
(******************************************************************************)

type color = | White | Black

(* [piece_rank] represents a chess piece type.
 * Both the king and the rook pieces have a flag for whether or not they have
 * already moved. The pawn contains two flags, one for whether or not they have
 * moved, and the other for whether or not they have advanced two spaces before.
 *)
type piece_rank =
  | King of bool
  | Queen
  | Rook of bool
  | Knight
  | Bishop
  | Pawn of bool * bool

type piece = color * piece_rank

type position = int * int

type move = position * position

type move_list = (piece * move) list

type last_move = (piece * move) option

type board = (position * piece) list * (position * piece) list

type check = | Black_Check | White_Check | No_Check

type end_game = | Checkmate | Stalemate


(******************************************************************************)
(*************************** BOARD INITIALIZATION *****************************)
(******************************************************************************)


(* [setup_front color rank count lst]
 * Helper to init_board. Returns a list of pawns of a given color set up in
 * their initial positions.
 * [c]: color of pawns to set up
 * [f]: file of current pawn to set up
 * [acc]: an accumulator for the list of pawns *)
let rec setup_front c f acc =
  let r = match c with | Black -> 7 | White -> 2 in
  let pos = (f,r) in
  if f <= 8
  then setup_front c (f+1) ((pos, (c, Pawn (false,false)))::acc)
  else acc

(* [setup_back c r]
 * Helper to init_board. Returns a list of all non-pawn pieces of a given color
 * set up in their initial positions.
 * [c]: color of pieces to set up *)
and setup_back c r =
  let r = match c with | Black -> 8 | White -> 1 in
  [ ((1, r), (c, Rook false));
    ((8, r), (c, Rook false));
    ((2, r), (c, Knight));
    ((7, r), (c, Knight));
    ((3, r), (c, Bishop));
    ((6, r), (c, Bishop));
    ((4, r), (c, Queen));
    ((5, r), (c, King false)); ]

(* [init_board]
 * A board with all pieces in their initial positions. Returns a tuple of lists
 * of pieces and their positions. The first list is the black pieces, while the
 * second is the white. *)
let init_board = (
      List.rev_append
        (setup_front Black 1 [])
        (setup_back  Black 8),
      List.rev_append
        (setup_front White 1 [])
        (setup_back  White 1))


(******************************************************************************)
(***************************** ASCII CONVERSION *******************************)
(******************************************************************************)

(* [piece_string p c]
 * Taken from https://github.com/shrumo/chess-engine
 * Coverts pieces to strings of their unicode counterparts.
 * [c]: color of the piece *)
let piece_string p c =
  match p with
  | Pawn _ -> if c = Black then "♙" else "♟"
  | Rook _ -> if c = Black then "♖" else "♜"
  | Knight -> if c = Black then "♘" else "♞"
  | Bishop -> if c = Black then "♗" else "♝"
  | Queen -> if c = Black then "♕" else "♛"
  | King _ ->  if c = Black then "♔" else "♚"

(* [board_to_matrix b] *)
let board_to_matrix b =
  let bb = (fst b) @ (snd b) in
  let rec make brd mat =
    match brd with
    | [] -> mat
    | ((x,y), (c,p))::t ->
      let p = piece_string p c in
      let ind = string_of_int x ^ string_of_int y in
      make t [(ind,p)]@mat
  in make bb []

(* [get_axis coord]
 * Returns the file or rank of a given coordinate. Used to label the ASCII
 * board.
 * [coord]: the coordinate *)
let get_axis coord =
  match coord with
  |(3,0) -> "A"
  |(5,0) -> "B"
  |(7,0) -> "C"
  |(9,0) -> "D"
  |(11,0) -> "E"
  |(13,0) -> "F"
  |(15,0) -> "G"
  |(17,0) -> "H"
  |(0,2) -> "8 "
  |(0,4) -> "7 "
  |(0,6) -> "6 "
  |(0,8) -> "5 "
  |(0,10) -> "4 "
  |(0,12) -> "3 "
  |(0,14) -> "2 "
  |(0,16) -> "1 "
  | _ -> ""

(* [print_board b]
 * Prints and ASCII representation of board [b].
 * [b]: the board to print  *)
let print_board b =
  let m = board_to_matrix b in
  let s = ref "" in
  for y2 = 0 to 18 do
    for x = 0 to 18 do
      let a = get_axis (x,y2) in
      if a <> "" then
        s := !s ^ a
      else
      if x <> 0 && y2 <> 0 then
        let y = (y2 -18) * -1 in
        let coord = string_of_int (x/2) ^ string_of_int (y/2) in
        if y mod 2 = 0 then
          if x mod 2 = 0 then
            if List.mem_assoc coord m then
              s := !s ^ (List.assoc coord m)
            else
              s := !s ^ " "
          else if y2 <> 18 then s := !s ^ "|" else s:= !s
        else
        if x mod 2 = 0 then
          s := !s ^ "_"
        else s := !s ^ " "
      else s := !s ^ " "
      done; s := !s ^ "\n";
  done; !s

(******************************************************************************)
(******************************** GAME LOGIC **********************************)
(******************************************************************************)


(****************************** GENERAL HELPERS *******************************)

(* [oppc c]
 * Given a color, return the opposite color.
 * [c]: the color to take the opposite of *)
let oppc c = match c with | Black -> White | White -> Black

(* [getpcs b c]
 * Retrieve a color's pieces from the board.
 * [b]: the board state to get pieces from
 * [c]: the color of pieces to retrieve *)
let getpcs b c = match c with | Black -> fst b | White -> snd b

(* [get_piece b pos]
 * Retrieve a piece at a given position on a board.
 * [b]: the board
 * [pos]: the position of the piece *)
let get_piece b pos =
  let b_ps = fst b in
  let w_ps = snd b in
  let rec loop pieces = begin
    match pieces with
    | [] -> None
    | ((x,y),piece)::t ->
      if ((x = (fst pos)) && (y = (snd pos))) then
        Some piece
      else
        loop t
  end in
  match loop b_ps with
  | Some p -> Some p
  | None -> loop w_ps

(* [getcheck c]
 * Given a color, return its respective check type.
 * [c]: the color check to retrieve *)
let getcheck c = match c with | Black -> Black_Check | White -> White_Check



(******************************** CHECK LOGIC *********************************)

(* [is_check b last_move c]
 * Returns whether a color is in check or not. Returns the color's check type if
 * so, [No_Check] if not.
 * [b]: the board state to check for check
 * [last_move]: the last move made on the board
 * [c]: the color to check for check *)
let rec is_check b last_move c =
  let opp_pieces = getpcs b (oppc c) in
  let k_pos =
    match c with
    | Black -> find_king (fst b)
    | White -> find_king (snd b) in
  if is_attacked b last_move opp_pieces k_pos
  then getcheck c
  else No_Check

(* [find_king ps]
 * Returns the position of the king in a list of pieces. Fails if not found, as
 * it should not be possible for the king to be removed from a board's list of
 * pieces.
 * [ps]: a color's set of pieces from a board state *)
and find_king ps =
  try
    fst (List.find
      (fun (c,(pos,piece)) -> match piece with
       | King _ -> true
       | _ -> false)
      ps)
  with Not_found -> failwith "Somehow captured a king."


(* Attack array and associated logic from Jonatan Pettersson at
 * http://mediocrechess.blogspot.com/2006/12/guide-attacked-squares.html *)

(* [attack_array]
 * Some mathemagical that allows easy lookup of whether or not a piece in one
 * position can attack another. *)
and attack_array =
   [0;0;0;0;0;0;0;0;0;5;0;0;0;0;0;0;2;0;0;0;     (* 0-19 *)
    0;0;0;5;0;0;5;0;0;0;0;0;2;0;0;0;0;0;5;0;     (* 20-39 *)
    0;0;0;5;0;0;0;0;2;0;0;0;0;5;0;0;0;0;0;0;     (* 40-59 *)
    5;0;0;0;2;0;0;0;5;0;0;0;0;0;0;0;0;5;0;0;     (* 60-79 *)
    2;0;0;5;0;0;0;0;0;0;0;0;0;0;5;6;2;6;5;0;     (* 80-99 *)
    0;0;0;0;0;0;0;0;0;0;6;4;1;4;6;0;0;0;0;0;     (* 100-119 *)
    0;2;2;2;2;2;2;1;0;1;2;2;2;2;2;2;0;0;0;0;     (* 120-139 *)
    0;0;6;3;1;3;6;0;0;0;0;0;0;0;0;0;0;0;5;6;     (* 140-159 *)
    2;6;5;0;0;0;0;0;0;0;0;0;0;5;0;0;2;0;0;5;     (* 160-179 *)
    0;0;0;0;0;0;0;0;5;0;0;0;2;0;0;0;5;0;0;0;     (* 180-199 *)
    0;0;0;5;0;0;0;0;2;0;0;0;0;5;0;0;0;0;5;0;     (* 200-219 *)
    0;0;0;0;2;0;0;0;0;0;5;0;0;5;0;0;0;0;0;0;     (* 220-239 *)
    2;0;0;0;0;0;0;5;0;0;0;0;0;0;0;0;0         ]  (* 240-256 *)

(* [fr2sq (f,r)]
 * Converts a [position] to 0x88 notation, which is required to use the attack
 * array.
 * [(f,r)]: the tuple representation of a position  *)
and fr_88 (f,r) = 16 * (r-1) + (f-1)

(* [att_constants]
 * More mathemagical numbers. Used in conjunction with the array to check if a
 * specific piece type can attack a space. *)
and att_constants = [
  ("k" , [1;3;4]);
  ("q" , [2;3;4;5]);
  ("r" , [1;2]);
  ("bw", [3;5]);
  ("bb", [4;5]);
  ("n" , [6]);
  ("p" , [3;4]);
]

(* [can_attack p (fa,ra) (fd,rd)]
 * Returns a bool for whether it is possible for a piece to attack a position on
 * the board, given its current position.
 * [p]: the piece that is attacking
 * [(fa,ra)]: the attacking position
 * [(fd,rd)]: the defending position *)
and can_attack p (fa,ra) (fd,rd) =
  let parray =
    match snd p with
    | King _ -> List.assoc "k" att_constants
    | Queen  -> List.assoc "q" att_constants
    | Rook _ -> List.assoc "r" att_constants
    | Bishop ->
      if fst p = Black
      then List.assoc "bb" att_constants
      else List.assoc "bw" att_constants
    | Knight -> List.assoc "n" att_constants
    | Pawn _ -> List.assoc "p" att_constants in
  let att_sq = fr_88 (fa,ra) in
  let def_sq = fr_88 (fd,rd) in
  let formula = def_sq - att_sq + 128 in
  List.mem (List.nth attack_array formula) parray

(* [is_attacked b last_move opp_ps d_pos]
 * Returns a bool for whether a position is actually attacked by a piece, given
 * the current state a the board. Essentially checks if the piece can actually
 * move into the position.
 * [b]: the current board
 * [last_move]: the last move made on the board
 * [opp_ps]: the attacking color's pieces
 * [d_pos]: the defending position *)
and is_attacked b last_move opp_ps d_pos =
  match opp_ps with
  | [] -> false
  | ((f,r), piece)::t ->
    if ((snd piece) != King true) && can_attack piece (f,r) d_pos
    then
      if List.mem d_pos (moves b last_move piece (f,r))
      then true
      else is_attacked b last_move t d_pos
    else is_attacked b last_move t d_pos


(***************************** PSEUDO MOVE LOGIC ******************************)

(* [moves b last_move p (f,r)]
 * Returns a list of psuedo moves for a given piece.
 * A pseudo move is one that is restricted only by the piece's movement rules.
 * [b]: the board state
 * [last_move]: the last move made on the board
 * [p]: the moving piece
 * [(f,r)]: the position of the moving piece *)
and moves b last_move p (f,r) =
  match (snd p) with
  | King moved -> moves_k b last_move (fst p) moved (f,r)
  | Queen -> moves_q b (fst p) (f,r)
  | Rook _ -> moves_r b (fst p) (f,r)
  | Knight -> moves_n b (fst p) (f,r)
  | Bishop -> moves_b b (fst p) (f,r)
  | Pawn (moved,advanced) -> moves_p b last_move (fst p) (moved,advanced) (f,r)

(* [is_occupied b (f,r)]
 * Checks whether a space is occupied by a piece on a board. Returns an option
 * containing the color of the piece if occupied.
 * [b]: the board
 * [(f,r)]: the position to check for occupancy *)
and is_occupied b (f,r) =
  if List.mem_assoc (f,r) (fst b) then Some Black
  else if List.mem_assoc (f,r) (snd b) then Some White
  else None

(* [moveable_space b c (f,r)]
 * Checks whether a space is can be moved into, i.e. it is in bounds on the
 * board and either is unoccupied or contains a piece of the opposing color.
 * [b]: the board
 * [c]: the color of the moving piece
 * [(f,r)]: the space to check for moveability *)
and moveable_space b c (f,r) =
  if 1<=f && f<=8 && 1<=r && r<=8
  then
    match is_occupied b (f,r) with
    | Some color -> if c != color then [(f,r)] else []
    | None -> [(f,r)]
  else []

(* [check_dir b c (f,r) (dx,dy) lst]
 * Returns a list of moveable spaces in a given direction. Used for sliding
 * pieces.
 * [b]: the board
 * [c]: the color of the moving piece
 * [(f,r)]: the position of the moving piece
 * [(dx,dy)]: the direction to check, in terms of change in x and y coordinates
 *    e.g. diagonally up right is (1,1), down is (0,-1), etc
 * [lst]: an accumulator list
 *)
and check_dir b c (f,r) (dx,dy) lst =
  let new_space = (f+dx,r+dy) in
  match moveable_space b c new_space with
  | [] -> lst
  | h::t -> check_dir b c new_space (dx,dy) (h::lst)

(* [moves_k b last_move c moved (f,r)]
 * Returns a list of spaces a king can move to given the board and its position.
 * [b]: the board
 * [last_move]: the last move made on the board
 * [c]: the color of the piece
 * [moved]: a flag for whether or not the king has moved yet
 * [(f,r)]: the position of the piece *)
and moves_k b last_move c moved (f,r) =
  let dirs = [(f+1,r);(f-1,r);(f,r+1);(f,r-1);
              (f+1,r+1);(f-1,r+1);(f+1,r+1);(f-1,r-1)] in
  let moves = List.fold_left (fun l d -> (moveable_space b c d)@l) [] dirs in

  let castle =
    if moved
    then []
    else castling b last_move c moved (f,r) in
  List.rev_append moves castle

(* [castling b last_move c moved (f,r)]
 * Returns a list of castling spaces.
 * A king is allowed to castle if it is not in check, it and the castling rook
 * have not moved yet, and the spaces between them are neither occupied nor
 * under attack.
 * [b]: the board
 * [last_move]: the last move made on the board
 * [c]: the color of the piece
 * [moved]: a flag for whether or not the king has moved yet
 * [(f,r)]: the position of the piece *)
and castling b last_move c moved (f,r) =
  let pcs = getpcs b c in
  let oppcs = getpcs b (oppc c) in
  let rrook =
    try Some (List.assoc (f+3,r) pcs)
    with Not_found -> None in
  let rcastle =
    match rrook with
    | Some (_, Rook false) ->
      if is_occupied b (f+1,r) = None &&
         not (is_attacked b last_move oppcs (f+1,r)) &&
         is_occupied b (f+2,r) = None &&
         not (is_attacked b last_move oppcs (f+2,r))
      then [(f+2,r)] else []
    | _ -> [] in
  let lrook =
    try Some (List.assoc (f-4,r) pcs)
    with Not_found -> None in
  let lcastle =
    match lrook with
    | Some (_, Rook false) ->
      if is_occupied b (f-1,r) = None &&
         not (is_attacked b last_move oppcs (f-1,r)) &&
         is_occupied b (f-2,r) = None &&
         not (is_attacked b last_move oppcs (f-2,r)) &&
         is_occupied b (f-3,r) = None &&
         not (is_attacked b last_move oppcs (f-3,r))
      then [(f-2,r)] else []
    | _ -> [] in
  rcastle @ lcastle

(* [moves_r b c (f,r)]
 * Returns a list of spaces a rook can move to given the board and its position.
 * [b]: the board
 * [c]: the color of the piece
 * [(f,r)]: the position of the piece *)
and moves_r b c (f,r) =
  let n  = check_dir b c (f,r) (1 , 0) [] in
  let s  = check_dir b c (f,r) (-1, 0) [] in
  let e  = check_dir b c (f,r) (0 , 1) [] in
  let w  = check_dir b c (f,r) (0 ,-1) [] in
  List.fold_left List.rev_append [] [n;s;e;w]

(* [moves_b b c (f,r)]
 * Returns a list of spaces a bishop can move to given the board and its
 * position.
 * [b]: the board
 * [c]: the color of the piece
 * [(f,r)]: the position of the piece *)
and moves_b b c (f,r) =
  let nw = check_dir b c (f,r) (-1, 1) [] in
  let ne = check_dir b c (f,r) ( 1, 1) [] in
  let sw = check_dir b c (f,r) (-1,-1) [] in
  let se = check_dir b c (f,r) ( 1,-1) [] in
  List.fold_left List.rev_append [] [nw;ne;sw;se]

(* [moves_q b c (f,r)]
 * Returns a list of spaces a queen can move to given the board and its
 * position.
 * [b]: the board
 * [c]: the color of the piece
 * [(f,r)]: the position of the piece *)
and moves_q b c (f,r) =
  List.rev_append (moves_r b c (f,r)) (moves_b b c (f,r))

(* [moves_n b c (f,r)]
 * Returns a list of spaces a knight can move to given the board and its
 * position.
 * [b]: the board
 * [c]: the color of the piece
 * [(f,r)]: the position of the piece *)
and moves_n b c (f,r) =
  let dxy = [(2,1);(2,-1);(-2,1);(-2,-1);
             (1,2);(-1,2);(1,-2);(-1,-2)] in
  let rec loop (f,r) l acc =
    match l with
    | [] -> acc
    | (dx,dy)::t ->
      begin
        let new_space = (f+dx,r+dy) in
        loop (f,r) t ((moveable_space b c new_space) @ acc)
      end in
  loop (f,r) dxy []

(* [moves_p b last_move c (m,a) (f,r)]
 * Returns a list of spaces a pawn can move to given the board and its position.
 * [b]: the board
 * [last_move]: the last move made on the board
 * [c]: the color of the piece
 * [(m,a)]: a pawn's movement flags; see type definition for piece_rank above
 * [(f,r)]: the position of the piece *)
and moves_p b last_move c (m,a) (f,r) =
  let inc =
    if c = Black then -1 else 1 in

  let forward = if is_occupied b (f,r+inc) = None then [(f,r+inc)] else [] in
  let forward_left =
    match is_occupied b (f-1,r+inc) with
    | Some color -> if color = oppc c then [(f-1,r+inc)] else []
    | _ -> [] in
  let forward_right =
    match is_occupied b (f+1,r+inc) with
    | Some color -> if color = oppc c then [(f+1,r+inc)] else []
    | _ -> [] in

  let two_sq = if m || a then [] else moveable_space b c (f,r+2*inc) in
  let en_pass =
    match last_move with
    | None -> []
    | Some lm -> enpass_valid lm (f,r) inc in
  forward @ forward_left @ forward_right @ two_sq @ en_pass

(* [enpass_valid (p, ((f1,r1),(f2,r2))) (f,r) inc]
 * Returns a space if a pawn is allowed to en passant, i.e:
 * - the last move was made by an opponent pawn advancing two spaces
 * - the opponent pawn is in an adjacent file and the same rank
 * - the space is unoccupied
 * [p]: the piece that maide the last move
 * [(f1,r1)]: the initial position of the piece that made the last move
 * [(f2,r2)]: the final position of the piece that made the last move
 * [(f,r)]: the position of the piece making the current move
 * [inc]: the change in rank that for advancement, i.e. -1 for black, 1 for
 *    white *)
and enpass_valid (p, ((f1,r1),(f2,r2))) (f,r) inc =
  match snd p with
  | Pawn (_, true) ->
      if (abs (r2-r1) = 2) && (r2 = r) && ((abs (f-f2) = 1)) then [(f2,r+inc)]
      else []
  | _ -> []


(**************************** BOARD UPDATE LOGIC ******************************)

(* [all_moves b last_move c]
 * Returns a list of all possible moves by a given color.
 * [b]: the board
 * [last_move]: the last move made on the board
 * [c]: the moving color *)
let all_moves b last_move c =
  let ps = getpcs b c in
  let rec loop pieces move_lst =
    match pieces with
    | [] -> move_lst
    | (i_pos, pce)::t ->
      begin
        let pmoves = List.map
                  (fun f_pos -> (i_pos,f_pos)) (moves b last_move pce i_pos) in
        let new_ml = List.rev_append pmoves move_lst in
        loop t new_ml
      end in
  loop ps []

(* [legal_moves b last_move c]
 * Returns a list of all legal moves by a given color. A legal move does not put
 * one's own king in check.
 * [b]: the board
 * [last_move]: the last move made on the board
 * [c]: the moving color *)
let rec legal_moves b last_move c =
  let mvs = all_moves b last_move c in
  let rec loop move_lst legal_lst =
    match move_lst with
    | [] -> legal_lst
    | move::t ->
      begin
        let b' = update_board b last_move c move in
        if is_check b' last_move c = getcheck c
        then loop t legal_lst
        else loop t ((move, b')::legal_lst)
      end in
  loop mvs []

(* [update_board b last_move c m]
 * Given a board and a move, return a new board that is the result of making the
 * move.
 * [b]: the old board
 * [last_move]: the last move made on the board
 * [c]: the moving color
 * [m]: the move to be made *)
and update_board b last_move c m =
  let i_pos = fst m in
  let f_pos = snd m in
  let ps = getpcs b c in
  let opps = getpcs b (oppc c) in
  let piece =
    match c with
    | Black -> List.assoc i_pos (fst b)
    | White -> List.assoc i_pos (snd b) in

  let piece' = update_piece_bool piece i_pos f_pos in
  let rm_pc = List.remove_assoc i_pos ps in
  let add_pc = (f_pos,piece')::rm_pc in
  let pcs' = update_castle piece' add_pc i_pos f_pos in

  let opps' = update_capture b opps piece' c i_pos f_pos in
  if c = Black
  then (pcs', opps')
  else (opps', pcs')

(* [update_piece_bool piece i_pos f_pos]
 * Updates a piece's movement flags, if they have them.
 * [p]: the moving piece
 * [i_pos]: the piece's initial position
 * [f_pos]: the piece's final position *)
and update_piece_bool p i_pos f_pos =
  match p with
    | c, King _ -> (c, King true)
    | c, Rook _ -> (c, Rook true)
    | c, Pawn _ ->
      if abs ((snd i_pos) - (snd f_pos)) = 2
      then (c, Pawn (true, true))
      else (c, Pawn (true, false))
    | _ -> p

(* [update_castle p pcs (fi,ri) (ff,rf)]
 * Updates a list of pieces in the event of castling to move the rook.
 * [p]: the moving piece; castling only occurs if this is a king
 * [pcs]: the list of pieces of the moving color
 * [(fi,ri)]: the piece's initial position
 * [(ff,rf)]: the piece's final position *)
and update_castle p pcs (fi,ri) (ff,rf) =
  match p with
  | _, King _ ->
    if ff-fi = 2 then
      let rm_rk = List.remove_assoc (8,ri) pcs in
      ((ff-1,ri),(fst p, Rook true))::rm_rk
    else if ff-fi = -2 then
      let rm_rk = List.remove_assoc (1,ri) pcs in
      ((ff+1,ri),(fst p, Rook true))::rm_rk
    else pcs
  | _ -> pcs

(* [update_capture b opps p c (fi,ri) (ff,rf)]
 * Updates a list of pieces to adjust for any capturing.
 * [b]: the board
 * [opps]: the opponent's pieces
 * [p]: the moving piece
 * [c]: the moving color
 * [(fi,ri)]: the moving piece's initial position
 * [(ff,rf)]: the moving piece's final position *)
and update_capture b opps p c (fi,ri) (ff,rf) =
  match p with
  | _, Pawn _ ->
    let inc = match c with | Black -> -1 | White -> 1 in
    if ri+inc = rf && (abs (ff-fi)) = 1 then
      if is_occupied b (ff,rf) = None
      (* en passant *)
      then List.remove_assoc (ff, rf-inc) opps
      (* regular capture *)
      else List.remove_assoc (ff,rf) opps
    else opps
  | _ -> List.remove_assoc (ff,rf) opps


(* [make_move b c last_move m leg_mves]
 * Updates a board with a legal move. Returns the new board, as well as whether
 * or not this move puts the opponent in check.
 * [b]: the board
 * [c]: the moving color
 * [last_move]: the last move made on the board
 * [m]: the move to make
 * [leg_mves]: a list of legal moves on the board for the given color *)
let make_move b c last_move m leg_mves =
  try
    let b' = List.assoc m leg_mves in
    (b', is_check b' last_move (oppc c))
  with Not_found -> (b, No_Check)

(* [promote b c last_move file newp]
 * Updates a board for promotion by replacing the promoting pawn in the piece
 * list with either a queen, rook, bishop, or knight. Returns
 * [b]: the board
 * [c]: the color piece to promote
 * [last_move]: the last move made on the board
 * [file]: the file of the pawn to promote
 * [newp]: a string representing the piece type to promote to*)
let promote b c last_move file newp =
  let endrank = match c with | Black -> 1 | White -> 8 in
  let pcs = getpcs b c in
  let pcs' = List.remove_assoc (file,endrank) pcs in
  let piece' =
    match newp with
    | "q" -> (c, Queen)
    | "r" -> (c, Rook true)
    | "b" -> (c, Bishop)
    | "n" -> (c, Knight)
    | _ -> failwith "Passed invalid string to promote." in
  let b' =
    match c with
    | Black -> (((file,endrank), piece')::pcs'), getpcs b (oppc c)
    | White -> getpcs b (oppc c), (((file,endrank), piece')::pcs') in
  let check = is_check b' last_move (oppc c) in
  (b', check)


(*************************************************************)
(**********************ALGEBRAIC NOTATION*********************)
(*************************************************************)

(* Debugging functions *)
let piece_to_string p =
  let color = match fst p with | White -> "white" | Black -> "black" in
  let rank =
    match snd p with
    | King(_) -> "King"
    | Queen -> "Queen"
    | Rook(_) -> "Rook"
    | Knight -> "Knight"
    | Bishop -> "Bishop"
    | Pawn(_) -> "Pawn"
  in
  Printf.sprintf "<Color: %s\tRank: %s>" color rank

(* Prints all elements in a (position, piece) list *)
let print_piecelst lst =
  List.iter
    (fun ((f,r), piece) ->
       Printf.printf "Pos (f,r): (%d,%d)\tPiece: %s\n" f r (piece_to_string piece))
    lst

(* Helper functions *)
(* For to_algno     *)
let rows = [|"a";"b";"c";"d";"e";"f";"g";"h"|]
let index_of arr elem =
  let rec index_of' arr i =
    if Array.length arr <= i then -1
    else if arr.(i) = elem then i
    else index_of' arr (i+1)
  in
  index_of' arr 0

let extract = function
  | Some (x) -> x
  | _ -> failwith "extract failed! Check Algebraic notation functions."

(* [can_move b lm cpos piece tpos]
 * Returns True if piece [piece] can move to target position [tpos]
 * given the state of the board as [b], the last_move as [lm], the
 * current position of [piece] as [cpos]
 *
 * Use in List.filter for converting to_algno *)
let can_move b lm cpos piece tpos =
  moves b lm piece cpos |> List.mem tpos

(* Returns the letter abbreviation for a piece rank [prank] *)
let abbrev prank =
  match prank with
  | King(_) -> "K"
  | Queen -> "Q"
  | Rook(_) -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Pawn(_) -> "" (* Empty abbreviation in SAN movetext *)

(* Returns true if move [m] is a capture given the board state
 * as [b] and the color of the piece doing move [m] as [c] *)
let is_capture b c m =
  match is_occupied b (snd m) with
  | Some(c) -> true
  | _ -> false

(* True if move [m] is a castle move, given the piece rank as [m_rank] *)
let is_castle m m_rank =
  match m_rank with
  | King(false) ->
    let ((sf, sr), (tf, tr)) = m in
    abs(tf - sf) > 1
  | _ -> false

(* Assumes: [m] is a move made by a KING piece *)
let castle_str m =
  let ((sf, sr), (tf, tr)) = m in
  if abs(tf - sf) = 2 then "O-O"
  else "O-O-O"

(* [is_check_bool b lm o_color m]
 * Returns true if move [m] will result in a check for the other
 * side, whose color is denoted [o_color]
 * [b] - board prior [m], lm] - last move, [o_color] - opponent color *)
let is_check_bool b lm o_color m =
  let b' = update_board b lm (oppc o_color) m in
  let m_piece = m |> fst |> get_piece b |> extract in
  let lm' = Some (m_piece, m) in
  match is_check b' lm' o_color with
  | No_Check -> false
  | _ -> true

(* [is_checkmate b lm o_color m]
 * Returns true if move [m] will result in a checkMATE for the other
 * side, whose color is denoted [o_color]
 * [b] - board prior [m], lm] - last move, [o_color] - opponent color *)
let is_checkmate b lm o_color m =
  let b' = update_board b lm (oppc o_color) m in
  let m_piece = m |> fst |> get_piece b |> extract in
  let lm' = Some (m_piece, m) in

  (* note: is_check_bool already updates its board - do not feed it [b'] or [lm']! *)
  is_check_bool b lm o_color m &&
  (List.length (legal_moves b' lm' o_color) = 0)

(* [ambiguous pcs m_rank m]
 * Returns a non-empty list if the move [m], made by piece whose rank is
 * [m_rank] represents an ambiguous move given that all pieces of the same
 * color in [pcs] can also move to the same target destination in [snd m]
 *
 * It is considered ambiguous if any piece in [pcs] is the same rank as [m_rank]
 * The non-empty list will contain those ambiguity-inducing pieces *)
let ambiguous pcs m_rank m =
  List.filter
    (fun (pos, piece) ->
       (snd piece) = m_rank && pos <> (fst m))
    pcs

(* [disambiguate o_piece_lst m_piece m]
 * Returns a string to disambiguate moves to [m] that all pieces in [o_piece_lst]
 * and [m_piece] can make. The rules for disambiguation are as follows...
 *
 * In order of preference, provide:
 *   1) the file letter of [m_piece]
 *   2) the numerical rank of [m_piece]
 *   3) the exact file-rank of [m_piece]
 * as necessary to disambiguate [o_piece_lst] from [m_piece]
 *
 * Assumes that there actually IS ambiguity to begin with! That is,
 * it does NOT check that a pcs in [o_piece_lst] can actually move to [m]! *)
let disambiguate o_piece_lst m_piece m =
  let (sf, sr) = m |> fst in

(* debug
  print_piecelst o_piece_lst;
  Printf.printf "m_piece: %s\t(sf, sr): (%d,%d)\n" (piece_to_string m_piece) sf sr;
*)

  let file_unambiguous =
    List.for_all
      (fun (pos, _) -> (fst pos) <> sf)  (* No files are identical *)
      o_piece_lst
  in
  let rank_unambiguous =
    List.for_all
      (fun (pos, _) -> (snd pos) <> sr)  (* No ranks are identical *)
      o_piece_lst
  in

  (* In order of preference. *)
  if file_unambiguous then
    rows.(sf-1)
  else if rank_unambiguous then
    string_of_int sr
  else
    rows.(sf-1) ^ (string_of_int sr)


(* Helper functions *)
(* For from_algno   *)

(* REGEX patterns for each component of algebraic notation
 * Each X_gp is a regex group to match one possible componenent of
 * algebraic chess notations. Combined, they form the full regex
 * pattern for algebraic chess notation. *)
let abbrev_gp = {|\(K\|Q\|R\|N\|B\)|}
let disamb_gp = {|\([a-h]\|[1-8]\|[a-h][1-8]\)|}
let capture_gp = {|\(x\)|}
let pos_gp = {|\([a-h][1-8]\)|}
let promote_gp = {|\(=Q\|=R\|=N\|=B\)|}
let check_or_mate_gp = {|\(#\|\+\)|}
let castle_gp = {|\(O-O-O\|O-O\)|}
let notation_rx =
  regexp (abbrev_gp ^ "?" ^
          disamb_gp ^ "?" ^
          capture_gp ^ "?" ^
          pos_gp ^
          promote_gp ^ "?" ^
          check_or_mate_gp ^ "?" ^ {|\||} ^
          castle_gp)

let file_gp = {|\([a-h]\)|}
let rank_gp = {|\([1-8]\)|}
let file_rank_rx =           (* Use to match against disamb_gp *)
  regexp (file_gp ^ "?" ^
          rank_gp ^ "?")

(* [matched gp_n str] wraps around Str's [matched_group] to return the
 * empty string if a match is not found, but throws an exception
 * otherwise. *)
let matched gp_n str =
  try
    matched_group gp_n str
  with
  | Not_found -> ""
  | _ -> failwith "invalid [matched] arguments"

(* [filter_type rank_abbrev pcs]
 * Filters out all (position * piece) elements in [pcs] to only those
 * whose rank is the specified [rank_abbrev] (e.g. "Q" specifies Queen) *)
let filter_type rank_abbrev pcs =
  let filter_fun =
    match rank_abbrev with
    | "K" -> (fun (_, (_, x)) -> match x with | King(_) -> true | _ -> false)
    | "Q" -> (fun (_, (_, x)) -> match x with | Queen -> true | _ -> false)
    | "R" -> (fun (_, (_, x)) -> match x with | Rook(_) -> true | _ -> false)
    | "N" -> (fun (_, (_, x)) -> match x with | Knight -> true | _ -> false)
    | "B" -> (fun (_, (_, x)) -> match x with | Bishop -> true | _ -> false)
    | "" -> (fun (_, (_, x)) -> match x with | Pawn(_) -> true | _ -> false)
    | _ -> failwith "filter_type: Invalid rank_abbrev!"
  in
  List.filter filter_fun pcs

(* [filter_cpos disamb pcs]
 * Filters out all (position * piece) elements in [pcs] to only those
 * whose position matches the disambiguation string [disamb]
 * If no elements match or [disamb] is not validly formed, returns []
 *
 * NOTE: This should get you a list with 1 element in it, unless something
 * got screwed up... *)
let filter_cpos disamb pcs =
  ignore (string_match file_rank_rx disamb 0);
  let file = matched 1 disamb in
  let rank = matched 2 disamb in
  let df = if file <> "" then (file |> index_of rows) + 1 else -1 in
  let dr = if rank <> "" then int_of_string rank else -1 in

  if df <> -1 && dr <> -1 then
    List.filter
      (fun ((f, r), _) ->
        f = df && r = dr)
      pcs
  else
    List.filter
      (fun ((f, r), _) ->
         f = df || r = dr)
      pcs

(* Exposed functions *)
(*                   *)
let to_algno ?promote:(promote = None) lm b m =
  let cpos = m |> fst in
  let tpos = m |> snd in
  let (tf, tr) = tpos in

  (* Information about the piece at [cpos] *)
  let m_piece = cpos |> get_piece b |> extract in
  let m_color = m_piece |> fst in
  let o_color = m_color |> oppc in  (* opponent color *)
  let m_rank = m_piece |> snd in

  (* Check if [m] is a special move-type: Castle *)
  if is_castle m m_rank then
    castle_str m
  else
    (* Basic common components of the notation string *)
    let abbrev_str = abbrev m_rank in
    let tpos_str = rows.(tf-1) ^ (string_of_int tr) in
    let capture_str = if is_capture b m_color m then "x" else "" in
    let check_or_mate_str =
      if is_checkmate b lm o_color m  then "#"
      else if is_check_bool b lm o_color m then "+"
      else ""
    in

    (* Since we're not castling then we actually have to check for
     * ambiguity. Yes, there may be ambiguity even for promotions... orz *)

    (* Gets a list [other_pcs] of all same-colored pieces that can move to [tpos] *)
    let pieces_list = m_color |> getpcs b in
    let other_pcs =
      List.filter (fun (pos, piece) -> can_move b lm pos piece tpos) pieces_list
    in

    (* Disambiguation characters *)
    let ambiguous_pcs = ambiguous other_pcs m_rank m in
    let disamb_str =
      if List.length ambiguous_pcs > 0 then disambiguate ambiguous_pcs m_piece m
      else "" in
    (* ^^^ disambiguation complete ^^^*)

    (* Check if [m] is a special move-type: promotion *)
    let promote_str =
      if promote != None then
        let promote_type = extract promote in
        let promote_abbrev = abbrev promote_type in
        "=" ^ promote_abbrev
      else
        ""
    in

    (* Final string - TODO: comment out this print at some point... *)
    Printf.printf "abbrev: %s|disamb: %s|capture: %s|tpos: %s|promote: %s|checkormate: %s\n" abbrev_str disamb_str capture_str tpos_str promote_str check_or_mate_str;
    abbrev_str ^ disamb_str ^ capture_str ^ tpos_str ^ promote_str ^ check_or_mate_str

(* NOTE: Assumes that [lm] can indicate whose turn it is.
 * In other words, [s] is done by the OPPOSITE color of last_move [lm] *)
let from_algno pr_target lm b s =
  ignore (string_match notation_rx s 0);
  let abbrev = matched 1 s in  (* Empty string if pawn, otherwise "N" "B" "Q" etc. *)
  let disamb = matched 2 s in
  let capture = matched 3 s in
  let pos = matched 4 s in
  let promote = matched 5 s in
  let check_or_mate = matched 6 s in
  let castle = matched 7 s in

  (* debug prints *)
  Printf.printf "abbrev: %s|disamb: %s|capture: %s|pos: %s| promote: %s|check_or_mate: %s \n" abbrev disamb capture pos promote check_or_mate;

  (* Information about the state of the board *)

  (* Parses the matched strings to produce a move *)
  (* identify whose turn [s] belongs to *)
  let s_color =
    match lm with
    | None | Some ((Black, _), _) -> White
    | _ -> Black
  in

  (* Special case: Castle move *)
  if castle <> "" then
    let king_pos = s_color |> getpcs b |> find_king in
    let (_, kr) = king_pos in
    let castle_pos = if castle = "O-O-O" then 2 else 7 in
    (king_pos, (castle_pos, kr))
  else
    (* Promotion/regular move *)
    let tpos =
      ((pos.[0] |> String.make 1 |> index_of rows) + 1,
       pos.[1] |> String.make 1|> int_of_string)
    in

    (* Gets a list [other_pcs] of all same-colored pieces that can move to [tpos] *)
    let pieces_list = s_color |> getpcs b in
    let pcs =
      List.filter (fun (pos', piece) -> can_move b lm pos' piece tpos) pieces_list
    in

    (* Filter by type and [disamb] if necessary *)
    let filtered_pcs = pcs |> filter_type abbrev in
    let (cpos, _) =
      if List.length filtered_pcs = 1 then
        List.hd filtered_pcs
      else
        filtered_pcs |> filter_cpos disamb |> List.hd
    in

    (* If [s] is a promotion move, specify the promotion target with
     * the [promote] target to Some target, else None *)
    begin
      pr_target :=
          match promote with
          | "" -> None
          | "=Q" -> Some Queen
          | "=R" -> Some (Rook(true))
          | "=N" -> Some Knight
          | "=B" -> Some Bishop
          | _ -> failwith "from_algno: invalid [promote] target!"
    end;
    (cpos, tpos)
