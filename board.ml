type color = | White | Black

type piece_rank =
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

type piece = color * piece_rank

type position = int * int

type move = position * position

type move_history = move list

type board = (position * piece) list * (position * piece) list

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
    setup_front color rank (count+1) ((pos, (color, Pawn))::lst)
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

type check = | Black_Check | White_Check | No_Check

type end_game = | Checkmate | Stalemate

let is_check b m = failwith "unimplemented"

(* [moves p pos] returns a list positions that piece [p] can move to, given that
 * it is in position [pos] on the board. *)
let rec moves p (f,r) =
  match p with
  | King -> moves_k (f,r)
  | Queen -> moves_q (f,r)
  | Rook -> moves_r (f,r)
  | Knight -> moves_n (f,r)
  | Bishop -> moves_b (f,r)
  | Pawn -> moves_p (f,r)

and in_bounds (f,r) =
  if 1<=f && f<=8 && 1<=r && r<=8
  then [(f,r)] else []

and check_dir (f,r) (dx,dy) lst =
  let new_space = (f+dx,r+dy) in
  if in_bounds new_space = [] then lst
  else check_dir new_space (dx,dy) (new_space::lst)

and moves_k (f,r) =
  in_bounds (f+1,r) @
  in_bounds (f-1,r) @
  in_bounds (f,r+1) @
  in_bounds (f,r-1)

and moves_r (f,r) =
  let n  = check_dir (f,r) (1 , 0) [] in
  let s  = check_dir (f,r) (-1, 0) [] in
  let e  = check_dir (f,r) (0 , 1) [] in
  let w  = check_dir (f,r) (0 ,-1) [] in
  List.fold_left List.rev_append [] [n;s;e;w]

and moves_b (f,r) =
  let nw = check_dir (f,r) (-1, 1) [] in
  let ne = check_dir (f,r) ( 1, 1) [] in
  let sw = check_dir (f,r) (-1,-1) [] in
  let se = check_dir (f,r) ( 1,-1) [] in
  List.fold_left List.rev_append [] [nw;ne;sw;se]

and moves_q (f,r) =
  List.rev_append (moves_r (f,r)) (moves_b (f,r))

and moves_n (f,r) =
  let dxy = [(2,1);(2,-1);(-2,1);(-2,-1);
             (1,2);(-1,2);(1,-2);(-1,-2)] in
  let rec loop (f,r) l acc =
    match l with
    | [] -> acc
    | (dx,dy)::t -> loop (f,r) t ((in_bounds (f+dx,r+dy)) @ acc) in
  loop (f,r) dxy []

and moves_p (f,r) =
  in_bounds (f,r+1)

let legal_moves b c = failwith "unimplemented"

let end_type b = failwith "unimplemented"

let update_board b m = failwith "unimplemented"
