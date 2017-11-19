open Lymp

let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"

let move_piece game (x1,y1) (x2,y2) =
  get_string gui "move" [!game;Pytuple [Pyint x1;Pyint y1]; Pytuple [Pyint x2;Pyint y2]]

let rec highlight game tiles =
  match tiles with
  | [] -> !game
  | (x,y)::t ->
    game := Pyref(get_ref gui "highlight" [!game; Pyint x; Pyint y]);
    highlight game t

let openers opener_list =
  failwith "Unimplemented"

let history move_list =
  failwith "Unimplemented"

(* let rec update t = *)
let () =
  let game = ref (Pyref (get_ref gui "start_game" [])) in
  let update = ref (get_list gui "update_game" [!game; Pybool true]) in
  game := List.nth !update 0;

  while (true) do
    update := (get_list gui "update_game" [!game; Pybool true]);
    game := List.nth !update 0;

    if ((List.nth !update 1) = (Pybool true)) then begin
      let piece = get_string gui "get_piece" [!game] in
      print_endline piece;
      game := (highlight game [(0,0);(0,1);(0,2);(0,3)]);
    end
    else begin
      print_string "";
    end

  done;;

  close py
