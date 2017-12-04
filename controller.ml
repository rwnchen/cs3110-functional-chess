open Lymp
open Board

type gui_state = pyobj

type position = int * int

type move = position * position

type click = | Piece of position | Highlight of position | Empty of position | Noclick (*| Opener of string? | History of string? | *)

let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"

let print_color c =
  match c with
  | Black ->
    "Black"
  | White ->
    "White"


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

let update_history state history =
  let rec build_hist_list m_list acc =
    match m_list with
    | [] -> acc
    | (h,b)::t -> build_hist_list t ((Pystr h)::acc) in
  let hist_list = build_hist_list history [] in
  state := Pyref(get_ref gui "update_history" [!state; Pylist hist_list]);
  !state

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
  | [Pystr "piece"; Pylist [Pyint x; Pyint y]] ->
    (* print_int x; print_int y; print_endline "Piece"; *)
    Piece (x,y)
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
  let history = ref [] in

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
      | Highlight (x,y) ->
        begin
          match !last_click with
          | Piece (x',y') ->
            let leg_moves = legal_moves !board !last_move !c in
            let (new_b, check) = make_move !board !c !last_move ((x',y'),(x,y)) leg_moves in
            if new_b <> !board then begin
              let piece =
                match get_piece !board (x',y') with
                | Some p -> p
                | None -> (White,Queen) (*This will never happen we just need it
                                        so it type checks*)
              in
              (* print_endline (piece_string (snd piece) (fst piece)); *)
              last_move := Some (piece,((x',y'),(x,y)));
              let lst_move =
                match !last_move with
                | None -> "none"
                | Some (p,m) ->
                  let ps = piece_string (snd p) (fst p) in
                  print_endline (print_color (fst p));
                  match m with
                  | ((i,j),(i',j')) ->
                    let ps1 = String.make 1 (Char.chr(i+64)) in
                    let ps2 = String.make 1 (Char.chr(i'+64)) in
                    ps ^ ": " ^ ps1 ^ (string_of_int j) ^ " to " ^ ps2 ^ (string_of_int j')
                  | _ -> ps ^ "()" in

              print_endline lst_move;
              history := (lst_move,new_b)::(!history);


              (* print_int x'; print_int y'; print_string " moved to ";
              print_int x; print_int y; print_endline ""; *)
              board := new_b;
              guistate := move_piece guistate ((x',y'),(x,y));
              guistate := update_history guistate !history;

              begin
                match !c with
                | White -> c := Black;
                | Black -> c := White;
              end
            end
          else begin
            print_endline "Not a legal move";
          end
        | _ -> guistate := !guistate end
      | _ -> guistate := !guistate
    end
    else begin
      ()
    end

  done;;

  close py
