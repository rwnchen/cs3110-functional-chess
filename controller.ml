open Lymp
open Board
open Opener

type gui_state = pyobj

type position = int * int

type move = position * position

(* A click type so we know what happened on the front end *)
type click = | Piece of position
             | Highlight of position
             | Promote of (string*int) (* (piece name * column) *)
             | Empty of position
             | Opener
             | History of int (*index*)
             | Load
             | Save
             | Noclick (*| Opener of string? | History of string? | *)

(* These are basic values that are needed to connect to a python file *)
let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"

(* Prints the colors *)
let print_color c =
  match c with
  | Black ->
    "Black"
  | White ->
    "White"

let extract = function
  | Some (x) -> x
  | None -> failwith "extract: failed to extract!"

(* [move_piece state ((x1,y1),(x2,y2))] performs the move [((x1,y1),(x2,y2))],
   moveing from (x1,y1) to (x2,y2) in the gui [guistate]
   Returns:
   - Pyref : new guistate after the move
*)
let move_piece guistate ((x1,y1),(x2,y2)) =
  let pos1 = Pytuple [Pyint x1;Pyint y1] in
  let pos2 = Pytuple [Pyint x2;Pyint y2] in
  Pyref(get_ref gui "move" [guistate; pos1; pos2])

(* [highlight tile_list] sends [tile_list] as a list of positions to be
   highlighted in the [guistate].
   Returns:
   - Pyref : new guistate after the highlighting
*)
let rec highlight guistate tiles =
  let rec build_tile_list t_list acc =
    match t_list with
    | [] -> acc
    | (x,y)::t -> build_tile_list t (Pylist [Pyint x; Pyint y]::acc) in
  let tiles = build_tile_list tiles [] in
  Pyref(get_ref gui "highlight" [guistate; Pylist tiles])

(* [update_history guistate history] sends [move_list] to the gui to display
   the new [history]. [history] is a list of
   (string,board,guistate,move,color). Which represents all the data for a
   particular spot in history.
   Returns:
    - Pyref : new guistate after the updatding history
*)
let update_history guistate history =
  let rec build_hist_list m_list acc =
    match m_list with
    | [] -> acc
    | (s,b,_,_,_)::t -> build_hist_list t ((Pystr s)::acc) in
  let hist_list = build_hist_list history [] in
  Pyref(get_ref gui "update_history" [guistate; Pylist hist_list])

(* Calls popup function in gui for checkmates.
   Returns:
    - Pyref: new guistate after popup*)
let check_mate_popup guistate =
  Pyref(get_ref gui "check_mate_popup" [guistate])

(* Calls popup function in gui for stalemates.
   Returns:
    - Pyref: new guistate after popup*)
let stale_mate_popup guistate =
  Pyref(get_ref gui "stale_mate_popup" [guistate])

(* Sends an old guistate to the board which reverts the board back to that
   point in the history.
   Returns:
   - Pyref: new guistate after reverting
*)
let revert_gui guistate i =
  Pyref(get_ref gui "revert" [guistate; Pyint i])

(* Gets which piece was selected for promotion in the gui.
   Returns:
   - String : the piece that was selected (r,k,b,q) *)
let get_promotion guistate =
  get_string gui "get_promotion" [guistate]

(* Helper function which gets which tiles the game needs to highlight based on
   the current moves and selectedp piece at pos [(x,y)] *)
let rec highlight_from_legal_moves legal_moves (x,y) acc =
  match legal_moves with
  | [] -> acc
  | (((p1x,p1y),(p2x,p2y)),board)::t ->
    if (p1x,p1y) = (x,y) then
      highlight_from_legal_moves t (x,y) ((p2x,p2y)::acc)
    else
      highlight_from_legal_moves t (x,y) acc

(* gets the click event from the gui and processes it into a click type *)
let parse_click guistate =
  let event = get_list gui "get_click" [guistate] in
  match event with
  | [Pystr "promote";Pylist [Pyint col; Pyint _]] ->
    let piece_name = get_promotion guistate in
    Promote (piece_name,col)
  | [Pystr "history"; Pyint i ] ->
    History i
  | [Pystr "piece"; Pylist [Pyint x; Pyint y]] ->
    (* print_int x; print_int y; print_endline "Piece"; *)
    Piece (x,y)
  | [Pystr "empty"; Pylist [Pyint x; Pyint y]] -> Empty (x,y)
  | [Pystr "highlight"; Pylist [Pyint x; Pyint y]] -> Highlight (x,y)
  | _ ->
    print_endline "noclick";
    Noclick (*This will happen when the use clicks somewhere that has no
                   click event*)

(* gets the tail of the list after index n *)
let rec list_from lst n =
  match lst with
  | [] -> []
  | h::t ->
    if n = 0 then t
    else list_from t (n-1)

(* [extract_py_bool (Pybool b)] either returns [b] or False if the pyobj is not
   a bool for some reason.
*)
let extract_py_bool p =
  match p with
  | Pybool b -> b
  | _ -> false

(* Opener related functions *)

(* Sends the openings in opener_list to the gui
 * opener_list : (string * string * float * string list) list
 *
 * It sends a python list of lists, of the form:
 * [["ECO category", "opening name", white's winrate (out of 1.0), list of
   moves], ...]
 * e.g.
 * [ ["A00", "Name here", 0.12, ["h4", "e5", "Nf3"]], ["A01", ...] ...]*)
let update_openers guistate opener_list =
  let rec build_py_openers t_list acc =
    match t_list with
    | [] -> acc
    | (eco, name, winrate, seq)::t ->
      let py_seq = List.map (fun s -> Pystr s) seq in
      build_py_openers t (Pylist [Pystr eco; Pystr name; Pyfloat winrate; Pylist py_seq]::acc) in
  let py_openings = build_py_openers opener_list [] in
  Pyref(get_ref gui "update_openers" [guistate; Pylist py_openings])

let format_replies opening_book algno_history =
  try
    let best_replies = best_reply opening_book (List.rev !algno_history) 10 in
    let rec analyze_replies r acc =
      match r with
      | [] -> acc
      | h::t ->
        let seq = List.rev (h::!algno_history) in
        let opm = opening_meta opening_book seq in
        let name = opening_name opm in
        let winrate = white_winrate opm in
        let eco_c = eco_category opm in
        analyze_replies t ((eco_c, name, winrate, [h])::acc)
    in
    analyze_replies best_replies []
  with _ -> []

let () =
  let guistate = ref (Pyref (get_ref gui "start_game" [])) in
  let update = ref (get_list gui "update_game" [!guistate; Pybool true]) in
  let was_click = ref (extract_py_bool (List.nth !update 0)) in
  let game_running = ref (extract_py_bool (List.nth !update 1)) in
  let board = ref Board.init_board in
  (* let highlights = ref [] in *)
  let last_move = ref None in
  let last_click = ref Noclick in
  let c = ref White in
  let history = ref [] in
  let game_end = ref false in

  (* loads opener data *)
  let opening_book = init_openings () in

  (* Move history in Standard Algebraic Notation *)
  let algno_history = ref [] in


  while (!game_running) do (*Gameloop. TODO: replace true with endgame check *)
    update := (get_list gui "update_game" [!guistate; Pybool true]);
    was_click := (extract_py_bool (List.nth !update 0));
    game_running := (extract_py_bool (List.nth !update 1));

    (* check if there was a click*)
    if (!was_click && not !game_end) then begin
      (* retrieve the click and parses it into the type defined above *)
      let click = parse_click !guistate in
      match click with
      (* Each button will have it's own match case and we can do things based
         on what is passed back *)
      | History i ->
        let (_,b,guist,lstmv,color) = List.nth !history i in
        let hist = list_from !history i in
        board := b;
        guistate := revert_gui guist i;
        last_move := lstmv;
        c := color;
        history := hist;

        algno_history := list_from !algno_history i;
        let analysis = format_replies opening_book algno_history in
        guistate := update_openers !guistate analysis;

        guistate := update_history !guistate !history;
      | Promote (piece_name,col) ->
        print_endline piece_name;
        let color = match !c with
          | White -> Black
          | Black -> White in
        (* need to flip color since color switches after move is done *)
        let (new_b,check) = promote !board color !last_move col piece_name in
        board := new_b;
      | Piece (x,y) ->
        let leg_moves = legal_moves !board !last_move !c in
        let highlights = highlight_from_legal_moves leg_moves (x,y) [] in
        let piece =
          match get_piece !board (x,y) with
          | Some p -> p
          | None -> (White,Queen) (*This will never happen we just need it
                                  so it type checks*)
        in
        guistate := highlight !guistate highlights;
        last_click := click;
      | Highlight (x,y) ->
        begin
          match !last_click with
          | Piece (x',y') ->
            let leg_moves = legal_moves !board !last_move !c in
            let (new_b, check) = make_move !board !c !last_move ((x',y'),(x,y)) leg_moves in
            if new_b <> !board then begin
              let piece =
                match get_piece new_b (x,y) with
                | Some p -> p
                | None -> (White,Queen) (*This will never happen we just need it
                                        so it type checks*)
              in
              (* print_endline (piece_string (snd piece) (fst piece)); *)

              (* Try to append to the Standard Algebraic Notation history *)
              begin
                try
                  let lm = !last_move in
                  let b = !board in
                  let m = ((x',y'),(x,y)) in
                  algno_history := (to_algno lm b m)::!algno_history;
                  print_endline (List.hd !algno_history)
                with _ -> ();
              end;

              last_move := Some (piece,((x',y'),(x,y)));
              let lst_move =
                match !last_move with
                | None -> "none"
                | Some (p,m) ->

                  (* We have to invert the colors because for some reason the
                    gui's colors are flipped. *)
                  let ps =
                    match (fst p) with
                    | White -> piece_string (snd p) Black
                    | Black -> piece_string (snd p) White in
                  match m with
                  | ((i,j),(i',j')) ->
                    let ps1 = String.make 1 (Char.chr(i+64)) in
                    let ps2 = String.make 1 (Char.chr(i'+64)) in
                    ps ^ ": " ^
                    ps1 ^ (string_of_int j) ^ " to "
                    ^ ps2 ^ (string_of_int j') in

              print_endline lst_move;
              history := (lst_move,!board,!guistate,!last_move,!c)::(!history);

              (* Computes and updates the new list of openers for display *)
              (* NOTE: algno_history is most recent first, but we need reverse*)
              let analysis = format_replies opening_book algno_history in
              guistate := update_openers !guistate analysis;


              (* print_int x'; print_int y'; print_string " moved to ";
              print_int x; print_int y; print_endline ""; *)
              (* Check for checkmate *)
              board := new_b;
              guistate := move_piece !guistate ((x',y'),(x,y));
              guistate := update_history !guistate !history;
              let lm_algno = List.hd !algno_history in
              try
                let _ = String.index lm_algno '#' in
                guistate := check_mate_popup !guistate;
                game_end := true;
              with
                Not_found ->
                begin
                  match !c with
                  | White -> c := Black;
                  | Black -> c := White;
                end
            end
          else begin
            print_endline "Not a legal move";
          end
        | _ -> guistate := !guistate
        end
      | _ -> guistate := !guistate
    end
    (* These are all the things the player can do if the game has ended *)
    else if (!was_click = true) then begin
      let click = parse_click !guistate in
      match click with
      (* Each button will have it's own match case and we can do things based
         on what is passed back *)
      | History i ->
        let (_,b,guist,lstmv,color) = List.nth !history i in
        let hist = list_from !history i in
        board := b;
        guistate := revert_gui guist i;
        last_move := lstmv;
        c := color;
        history := hist;

        algno_history := list_from !algno_history i;
        let analysis = format_replies opening_book algno_history in
        guistate := update_openers !guistate analysis;

        guistate := update_history !guistate !history;

        (* If history was rolled back, undo game_end *)
        game_end := if i >= 0 then false else !game_end;
      | _ -> guistate := !guistate
    end
    else begin
      ()
    end

  done;;

  close py
