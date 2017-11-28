(******************************************************************************)
(***************************** TYPE DECLARATIONS ******************************)
(******************************************************************************)

type color = | White | Black

type piece_rank =
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn of bool

type piece = color * piece_rank

type position = int * int

type move = position * position

type move_list = (piece * move) list

type move_history = {mutable ms : move_list}

type board = (position * piece) list * (position * piece) list

type check = | Black_Check | White_Check | No_Check

type end_game = | Checkmate | Stalemate


(******************************************************************************)
(*************************** BOARD INITIALIZATION *****************************)
(******************************************************************************)

let mh = {ms = []}

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
    setup_front color rank (count+1) ((pos, (color, Pawn (false)))::lst)
  else lst

and setup_back color rank =
  [ ((1, rank), (color, Rook));
    ((8, rank), (color, Rook));
    ((2, rank), (color, Knight));
    ((7, rank), (color, Knight));
    ((3, rank), (color, Bishop));
    ((6, rank), (color, Bishop));
    ((4, rank), (color, Queen));
    ((5, rank), (color, King)); ]


let init_board = ((setup_board Black), (setup_board White))


(*************************************************************)
(************************ GAME LOGIC *************************)
(*************************************************************)


(********************* PSUEDO-MOVE LOGIC *********************)

(* [moves p pos] returns a list positions that piece [p] can move to, given that
 * it is in position [pos] on board [b]. *)
let rec moves b mh p (f,r) =
  match (snd p) with
  | King -> moves_k b mh (fst p) (f,r)
  | Queen -> moves_q b (fst p) (f,r)
  | Rook -> moves_r b (fst p) (f,r)
  | Knight -> moves_n b (fst p) (f,r)
  | Bishop -> moves_b b (fst p) (f,r)
  | Pawn moved -> moves_p b mh (fst p) moved (f,r)

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

and moves_k b mh c (f,r) =
  let e = moveable_space b c (f+1,r) in
  let w = moveable_space b c (f-1,r) in
  let n = moveable_space b c (f,r+1) in
  let s = moveable_space b c (f,r-1) in

  let rank = match c with | Black -> 8 | White -> 1 in
  let castle = [] in (* TODO *)
  e @ w @ n @ s @ castle


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

and moves_p b mh c moved (f,r) =
  let inc =
    if c = Black then -1 else 1 in

  let forward = moveable_space b c (f,r+inc) in
  let forward_left = moveable_space b c (f-1,r+inc) in
  let forward_right = moveable_space b c (f+1,r+inc) in

  let two_sq = if moved then [] else [(f,r+2*inc)] in
  let en_pass =
    match mh.ms with
    | [] -> []
    | (p, ((f1,r1),(f2,r2)))::t ->
      if (snd p = Pawn true) && f1 = f2 then
        if r2-r1 = 2 then [(f1,r2-1)]
        else if r1-r2 = 2 then [(f1,r2+1)]
        else []
      else [] in
  forward @ forward_left @ forward_right @ two_sq @ en_pass


(************************ CHECK LOGIC ************************)

let rec is_check b c =
  let opp_pieces = match c with | Black -> snd b | White -> fst b in
  let k_pos =
    match c with
    | Black -> find_king (fst b)
    | White -> find_king (snd b) in
  if is_attacked b opp_pieces k_pos
  then
    match c with
    | Black -> Black_Check
    | White -> White_Check
  else No_Check


and find_king ps =
  try
    fst (List.find
      (fun (c,(pos,piece)) -> match piece with
       | King -> true
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
    | King   -> List.assoc "k" att_constants
    | Queen  -> List.assoc "q" att_constants
    | Rook   -> List.assoc "r" att_constants
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

and is_attacked b opp_ps d_pos =
  match opp_ps with
  | [] -> false
  | ((f,r), piece)::t ->
    if (snd piece) != King && can_attack piece (f,r) d_pos
    then
      if List.mem d_pos (moves b mh piece (f,r))
      then true (*  match c with | Black -> Black_Check | White -> White_Check *)
      else is_attacked b t d_pos
    else is_attacked b t d_pos


(********************* BOARD UPDATE LOGIC **********************)

let update_board b mh c m =
  let i_pos = fst m in
  let f_pos = snd m in
  let bl_ps = fst b in
  let wh_ps = snd b in
  let piece =
    match c with
    | Black -> List.assoc i_pos bl_ps
    | White -> List.assoc i_pos wh_ps in
  let selfcheck = match c with | Black -> Black_Check | White -> White_Check in
  let oppcheck = match c with | Black -> White_Check | White -> Black_Check in

  if List.mem f_pos (moves b mh piece i_pos)
  then
    let ps = if c = Black then bl_ps else wh_ps in
    let ps' = (List.remove_assoc i_pos ps) in
    let ps'' = (f_pos,piece)::ps' in
    let b' = if c = Black then (ps'', wh_ps) else (bl_ps, ps'') in

    if is_check b' c = selfcheck
    then (b , "Places king in check.")
    else if is_check b' c = oppcheck
    then
      let () = mh.ms <- ((piece,m)::mh.ms) in
      (b', "Check.")
    else let () = mh.ms <- ((piece,m)::mh.ms) in
      (b', "")
  else (b, "Not a possible move.")


let promote b (f,r) = failwith "unimplemented"

let end_type b = failwith "unimplemented"
