open Lymp
open Board

type gui_state = pyobj

type position = int * int

type move = position * position

type click = | Piece of position | Highlight of position | Empty of position | Noclick (*| Opener of string? | History of string? | *)

let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"


let move_piece state ((x1,y1),(x2,y2)) =
  let pos1 = Pytuple [Pyint x1;Pyint y1] in
  let pos2 = Pytuple [Pyint x2;Pyint y2] in
  state := Pyref(get_ref gui "move" [!state; pos1; pos2]);
  !state

let rec highlight state tiles =
  match tiles with
  | [] -> !state
  | (x,y)::t ->
    state := Pyref(get_ref gui "highlight" [!state; Pyint x; Pyint y]);
    highlight state t

let openers opener_list =
  failwith "Unimplemented"

let history move_list =
  failwith "Unimplemented"

let rec highlight_from_legal_moves legal_moves (x,y) acc =
  match legal_moves with
  | [] -> acc
  | (((p1x,p1y),(p2x,p2y)),board)::t ->
    if (p1x,p1y) = (x,y) then
      highlight_from_legal_moves t (x,y) ((p2x,p2y)::acc)
    else
      highlight_from_legal_moves t (x,y) acc

let parse_click guistate =
  let event = get_list gui "get_click" [guistate] in
  match event with
  | [Pystr "piece"; Pylist [Pyint x; Pyint y]] -> Piece (x,y)
  | [Pystr "empty"; Pylist [Pyint x; Pyint y]] -> Empty (x,y)
  | [Pystr "highlight"; Pylist [Pyint x; Pyint y]] -> Highlight (x,y)
  | _ ->
    print_endline "noclick";
    Noclick (*This will happen when the use clicks somewhere that has no
                   click event*)

let () =
  let guistate = ref (Pyref (get_ref gui "start_game" [])) in
  let update = ref (get_bool gui "update_game" [!guistate; Pybool true]) in
  let board = ref Board.init_board in
  (* let highlights = ref [] in *)
  let last_move = ref None in
  let last_click = ref Noclick in
  let c = ref White in

  while (true) do (*Gameloop. TODO: replace true with endgame check*)
    update := (get_bool gui "update_game" [!guistate; Pybool true]);
    if (!update = true) then begin
      let click = parse_click !guistate in
      match click with
      | Piece (x,y) ->
        let leg_moves = legal_moves !board !last_move !c in
        let highlights = highlight_from_legal_moves leg_moves (x,y) [] in
        guistate := highlight guistate highlights;
        last_click := click;
      | Highlight (x,y) -> begin
        match !last_click with
        | Piece (x',y') ->
          let leg_moves = legal_moves !board !last_move !c in
          let (new_b, check) = make_move !board !c !last_move ((x',y'),(x,y)) leg_moves in
          print_int x'; print_int y'; print_string " moved to ";
          print_int x; print_int y; print_endline "";
          board := new_b;
          guistate := move_piece guistate ((x',y'),(x,y));
          begin
            match !c with
            | White -> c := Black;
            | Black -> c := White;
          end
        | _ -> guistate := !guistate end
      | _ -> guistate := !guistate
    end
    else begin
      ()
    end

  done;;

  close py
