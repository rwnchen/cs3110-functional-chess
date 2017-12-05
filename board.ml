(******************************************************************************)
(***************************** TYPE DECLARATIONS ******************************)
(******************************************************************************)

type color = | White | Black

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

type move_history = {mutable ms : move_list}

type board = (position * piece) list * (position * piece) list

type check = | Black_Check | White_Check | No_Check

type end_game = | Checkmate | Stalemate


(******************************************************************************)
(*************************** BOARD INITIALIZATION *****************************)
(******************************************************************************)

(* let mh = {ms = []} *)

let rec setup_board color =
  match color with
  | Black ->
      List.rev_append
        (setup_front Black 7 1 [])
        (setup_back  Black 8)
  | White ->
      List.rev_append
        (setup_front White 2 1 [])
        (setup_back  White 1)

and setup_front color rank count lst =
  if count <= 8
  then
    let pos = (count, rank) in
    setup_front color rank (count+1) ((pos, (color, Pawn (false,false)))::lst)
  else lst

and setup_back color rank =
  [ ((1, rank), (color, Rook false));
    ((8, rank), (color, Rook false));
    ((2, rank), (color, Knight));
    ((7, rank), (color, Knight));
    ((3, rank), (color, Bishop));
    ((6, rank), (color, Bishop));
    ((4, rank), (color, Queen));
    ((5, rank), (color, King false)); ]


let init_board = ((setup_board Black), (setup_board White))

(*https://github.com/shrumo/chess-engine*)
let piece_string p color =
  match p with
  | Pawn (m,a) -> if color = Black then "♙" else "♟"
  | Rook b -> if color = Black then "♖" else "♜"
  | Knight -> if color = Black then "♘" else "♞"
  | Bishop -> if color = Black then "♗" else "♝"
  | Queen -> if color = Black then "♕" else "♛"
  | King b ->  if color = Black then "♔" else "♚"

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

(*************************************************************)
(************************ GAME LOGIC *************************)
(*************************************************************)


(************************** HELPERS **************************)

let oppc c = match c with | Black -> White | White -> Black
let getpcs (b:board) (c:color) = match c with | Black -> fst b | White -> snd b
let getcheck c = match c with | Black -> Black_Check | White -> White_Check


(************************ CHECK LOGIC ************************)

let rec is_check b last_move c =
  let opp_pieces = getpcs b (oppc c) in
  let k_pos =
    match c with
    | Black -> find_king (fst b)
    | White -> find_king (snd b) in
  if is_attacked b last_move opp_pieces k_pos
  then getcheck c
  else No_Check


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

and fr2sq (f,r) = 16 * (r-1) + (f-1)

and att_constants = [
  ("k" , [1;3;4]);
  ("q" , [2;3;4;5]);
  ("r" , [1;2]);
  ("bw", [3;5]);
  ("bb", [4;5]);
  ("n" , [6]);
  ("p" , [3;4]);
]

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
  let att_sq = fr2sq (fa,ra) in
  let def_sq = fr2sq (fd,rd) in
  let formula = def_sq - att_sq + 128 in
  List.mem (List.nth attack_array formula) parray

and is_attacked b last_move opp_ps d_pos =
  match opp_ps with
  | [] -> false
  | ((f,r), piece)::t ->
    if ((snd piece) != King true) && can_attack piece (f,r) d_pos
    then
      if List.mem d_pos (moves b last_move piece (f,r))
      then true (*  match c with | Black -> Black_Check | White -> White_Check *)
      else is_attacked b last_move t d_pos
    else is_attacked b last_move t d_pos


(********************* PSUEDO-MOVE LOGIC *********************)

(* [moves p pos] returns a list positions that piece [p] can move to, given that
 * it is in position [pos] on board [b]. *)
and moves b last_move p (f,r) =
  match (snd p) with
  | King moved -> moves_k b last_move (fst p) moved (f,r)
  | Queen -> moves_q b (fst p) (f,r)
  | Rook _ -> moves_r b (fst p) (f,r)
  | Knight -> moves_n b (fst p) (f,r)
  | Bishop -> moves_b b (fst p) (f,r)
  | Pawn (moved,advanced) -> moves_p b last_move (fst p) (moved,advanced) (f,r)

and in_bounds (f,r) = 1<=f && f<=8 && 1<=r && r<=8


and is_occupied b (f,r) =
  if List.mem_assoc (f,r) (fst b) then Some Black
  else if List.mem_assoc (f,r) (snd b) then Some White
  else None

and moveable_space b c (f,r) =
  if in_bounds (f,r)
  then
    match is_occupied b (f,r) with
    | Some color -> if c != color then [(f,r)] else []
    | None -> [(f,r)]
  else []

and check_dir b c (f,r) (dx,dy) lst =
  let new_space = (f+dx,r+dy) in
  if not (in_bounds new_space) then lst
  else
    match is_occupied b new_space with
    | Some color ->
      if c = color then lst
      else new_space::lst
    | None -> check_dir b c new_space (dx,dy) (new_space::lst)

and moves_k b last_move c moved (f,r) =
  let e = moveable_space b c (f+1,r) in
  let w = moveable_space b c (f-1,r) in
  let n = moveable_space b c (f,r+1) in
  let s = moveable_space b c (f,r-1) in

  let castle =
    if moved
    then []
    else castling b last_move c moved (f,r) in
  e @ w @ n @ s @ castle

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

and moves_r b c (f,r) =
  let n  = check_dir b c (f,r) (1 , 0) [] in
  let s  = check_dir b c (f,r) (-1, 0) [] in
  let e  = check_dir b c (f,r) (0 , 1) [] in
  let w  = check_dir b c (f,r) (0 ,-1) [] in
  List.fold_left List.rev_append [] [n;s;e;w]

and moves_b b c (f,r) =
  let nw = check_dir b c (f,r) (-1, 1) [] in
  let ne = check_dir b c (f,r) ( 1, 1) [] in
  let sw = check_dir b c (f,r) (-1,-1) [] in
  let se = check_dir b c (f,r) ( 1,-1) [] in
  List.fold_left List.rev_append [] [nw;ne;sw;se]

and moves_q b c (f,r) =
  List.rev_append (moves_r b c (f,r)) (moves_b b c (f,r))

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
    | None -> [(1,1)]
    | Some lm -> enpass_valid lm (f,r) inc in
  forward @ forward_left @ forward_right @ two_sq @ en_pass

and enpass_valid (p, ((f1,r1),(f2,r2))) (f,r) inc =
  match snd p with
  | Pawn (_, true) ->
      if (abs (r2-r1) = 2) && (r2 = r) && ((abs (f-f2) = 1)) then [(1,2)](* [(f2,r+inc)] *)
      else [(1,3)]
  | King _ -> [(1,4)]
  | Queen ->  [(1,5)]
  | Rook _ -> [(1,6)]
  | Bishop -> [(1,7)]
  | Knight -> [(1,8)]
  | _ -> [(2,1)]

(********************* BOARD UPDATE LOGIC **********************)

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

and update_piece_bool piece i_pos f_pos =
  match piece with
    | c, King _ -> (c, King true)
    | c, Rook _ -> (c, Rook true)
    | c, Pawn _ ->
      if abs ((snd i_pos) - (snd f_pos)) = 2
      then (c, Pawn (true, true))
      else (c, Pawn (true, false))
    | _ -> piece

and update_castle p pcs (fi,ri) (ff,rf) =
  match p with
  | _, King _ ->
    if ff-fi = 2
    then
      let rm_rk = List.remove_assoc (8,ri) pcs in
      ((ff-1,ri),(fst p, Rook true))::rm_rk
    else if ff-fi = -2
    then
      let rm_rk = List.remove_assoc (1,ri) pcs in
      ((ff+1,ri),(fst p, Rook true))::rm_rk
    else pcs
  | _ -> pcs

and update_capture b opps piece c (fi,ri) (ff,rf) =
  match piece with
  | _, Pawn _ ->
    let inc = match c with | Black -> -1 | White -> 1 in
    if ri+inc = rf && (abs (ff-fi)) = 1
    then pawn_capture b c opps (fi,ri) (ff,rf)
    else opps
  | _ -> List.remove_assoc (ff,rf) opps

and pawn_capture b c opps i_pos f_pos =
  (* match snd p with
  | Pawn (_,true) ->
    if (abs (r2-r1) = 2) && r2 = (snd i_pos)
    then List.remove_assoc (f2,r2) opps
    else List.remove_assoc f_pos opps
  | _ -> opps *)
  let inc = match c with | Black -> -1 | White -> 1 in
  if is_occupied b f_pos = None
  then List.remove_assoc ((fst f_pos), (snd f_pos)-inc) opps
  else List.remove_assoc f_pos opps
(*   if snd p = (Pawn (true,true)) && (abs ()) *)

let make_move b c last_move (m:move) (leg_mves:((move * board) list)) =
  try
    let b' = List.assoc m leg_mves in
    (b', is_check b' last_move (oppc c))
  with Not_found -> (b, No_Check)


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
  (b, check)


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
(*                  *)
let rows = [|"a";"b";"c";"d";"e";"f";"g";"h"|]

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

  (* note: is_check_bool already updates its board - do not feed it [b']! *)
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

  print_piecelst o_piece_lst;
  Printf.printf "m_piece: %s\t(sf, sr): (%d,%d)\n" (piece_to_string m_piece) sf sr;

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

    (* Final string *)
    Printf.printf "abbrev: %s|disamb: %s|capture: %s|tpos: %s|promote: %s|checkormate: %s\n" abbrev_str disamb_str capture_str tpos_str promote_str check_or_mate_str;
    abbrev_str ^ disamb_str ^ capture_str ^ tpos_str ^ promote_str ^ check_or_mate_str

let from_algno lm b s =
  failwith "todo"
