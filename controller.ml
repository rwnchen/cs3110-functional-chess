open Lymp
open Board

type gui_state = pyobj

type position = int * int

type move = position * position

let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"

let move_piece state ((x1,y1),(x2,y2)) =
  let pos1 = Pytuple [Pyint x1;Pyint y1] in
  let pos2 = Pytuple [Pyint x2;Pyint y2] in
  state := Pyref(get_ref gui "move" [!state;pos1; pos2]);
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

(* let rec update t = *)
let () =
  let game = ref (Pyref (get_ref gui "start_game" [])) in
  let update = ref (get_list gui "update_game" [!game; Pybool true]) in
  game := List.nth !update 0;
  let board = ref Board.init_board in

  while (true) do
    update := (get_list gui "update_game" [!game; Pybool true]);
    game := List.nth !update 0;

    if ((List.nth !update 1) = (Pybool true)) then begin
      let py_pos = get_list gui "get_piece" [!game] in
      let (Pyint x) = List.nth py_pos 0 in
      let (Pyint y) = List.nth py_pos 1 in
      print_int x;
      print_int y;
      print_endline "";

      game := (highlight game [(x,y)]);
      match (Board.get_piece !board (x+1,y+1)) with
      | Some _ ->
        print_endline "Piece Clicked";
      | None ->
        print_endline "None";
    end
    else begin
      print_string "";
    end

  done;;

  close py
