(*http://pleac.sourceforge.net/pleac_ocaml/sockets.html*)

open Lymp
open Board

type gui_state = pyobj

type position = int * int

type move = position * position

type click = | Piece of position
             | Highlight of position
             | Promote of (string*int) (* (piece name * column) *)
             | Empty of position
             | Noclick (*| Opener of string? | History of string? | *)

let listen_address =
  if Array.length Sys.argv < 2
  then
    (Printf.printf "usage : server port\n"; exit 2)
  else
    Unix.inet_addr_of_string (Sys.argv.(1))

let port = 9000

let sockaddr = Unix.ADDR_INET (listen_address, port)

let interpreter = "python"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"

let print_color c =
  match c with
  | Black ->
    "Black"
  | White ->
    "White"

let move_piece guistate ((x1,y1),(x2,y2)) =
  let pos1 = Pytuple [Pyint x1;Pyint y1] in
  let pos2 = Pytuple [Pyint x2;Pyint y2] in
  Pyref(get_ref gui "move" [guistate; pos1; pos2])

let rec highlight guistate tiles =
  let rec build_tile_list t_list acc =
    match t_list with
    | [] -> acc
    | (x,y)::t -> build_tile_list t (Pylist [Pyint x; Pyint y]::acc) in
  let tiles = build_tile_list tiles [] in
  Pyref(get_ref gui "highlight" [guistate; Pylist tiles])

let openers opener_list =
  failwith "Unimplemented"

let update_history guistate history =
  let rec build_hist_list m_list acc =
    match m_list with
    | [] -> acc
    | (h,b)::t -> build_hist_list t ((Pystr h)::acc) in
  let hist_list = build_hist_list history [] in
  Pyref(get_ref gui "update_history" [guistate; Pylist hist_list])

let get_promotion guistate =
  get_string gui "get_promotion" [guistate]

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
  | [Pystr "promote";Pylist [Pyint col; Pyint y]] ->
    let piece_name = get_promotion guistate in
    Promote (piece_name,col)
  | [Pystr "piece"; Pylist [Pyint x; Pyint y]] ->
    (* print_int x; print_int y; print_endline "Piece"; *)
    Piece (x,y)
  | [Pystr "empty"; Pylist [Pyint x; Pyint y]] -> Empty (x,y)
  | [Pystr "highlight"; Pylist [Pyint x; Pyint y]] -> Highlight (x,y)
  | _ ->
    print_endline "noclick";
    Noclick (*This will happen when the use clicks somewhere that has no
                   click event*)

let coords_to_string x y x' y' =
  let coord2 = Char.escaped(Char.chr(x + 64))^(string_of_int y) in
  let coord1 = Char.escaped(Char.chr(x' + 64))^(string_of_int y') in
  coord1 ^ " " ^ coord2

let string_to_coords s =
  let space = (String.split_on_char ' ' s) in
  let (p1,p2) =  ((List.nth space 0),(List.nth space 1)) in
  let (x',y') = (Char.code(String.get p1 0)-64,int_of_char(String.get p1 1)-48) in
  let (x,y) = (Char.code(String.get p2 0)-64,int_of_char(String.get p2 1)-48) in
  (x',y',x,y)

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  let guistate = ref (Pyref (get_ref gui "start_game" [])) in
  let update = ref (get_bool gui "update_game" [!guistate; Pybool true]) in
  let board = ref Board.init_board in
  (* let highlights = ref [] in *)
  let last_move = ref None in
  let last_click = ref Noclick in
  let c = ref White in
  let history = ref [] in

  Unix.connect socket sockaddr;
  match Unix.fork () with
  | 0 -> let output = Unix.out_channel_of_descr socket in
    flush output
    (* let output = Unix.out_channel_of_descr socket in
    let coord = ref "" in

    while true do
      (*TODO: DO GAME LOGIC*)
      (*  (update := (get_bool gui "update_game" [!guistate; Pybool true]);
      if (!update = true) then begin
        let click = parse_click !guistate in
        match click with
        | Promote (piece_name,col)->
          let color = match !c with
            | White -> Black
            | Black -> White in
          let (new_b,check) = promote !board color !last_move col piece_name in
          if !board = new_b then
            print_endline "test";
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

                    (* We have to invert the colors because for some reason the
                       gui's codes are flipped. *)
                    let ps =
                      match (fst p) with
                      | White -> piece_string (snd p) Black
                      | Black -> piece_string (snd p) White in
                    match m with
                    | ((i,j),(i',j')) ->
                      let ps1 = String.make 1 (Char.chr(i+64)) in
                      let ps2 = String.make 1 (Char.chr(i'+64)) in
                      ps ^ ": " ^ ps1 ^ (string_of_int j) ^ " to " ^ ps2 ^ (string_of_int j') in

                print_endline lst_move;
                history := (lst_move,new_b)::(!history);


                (* print_int x'; print_int y'; print_string " moved to ";
                   print_int x; print_int y; print_endline ""; *)
                board := new_b;
                guistate := move_piece !guistate ((x',y'),(x,y));
                guistate := update_history !guistate !history;
                coord := coords_to_string x y x' y';
              end
              else begin
                print_endline "Not a legal move";
              end
            | _ -> guistate := !guistate end
        | _ -> guistate := !guistate
      end);*)
      output_string stdout "\n";
      flush output
       done*)
  | kidpid ->
    let input = Unix.in_channel_of_descr socket in
    try
      let go = ref true in
      while true do
        let line = input_line input in
        if line <> "" && !go = true then
          let (x',y',x,y) =  string_to_coords line in
          let leg_moves = legal_moves !board !last_move !c in
          let (new_b, check) = make_move !board !c !last_move ((x',y'),(x,y)) leg_moves in
          board := new_b;
          print_int x'; print_int y'; print_string " moved to ";
          print_int x; print_int y; print_endline "";
          guistate := move_piece !guistate ((x',y'),(x,y));
          go := false;
          output_string stdout line;
          output_string stdout "\n";
          flush stdout
        else
        (* guistate := update_history !guistate !history; *)
        output_string stdout line;
        output_string stdout "\n";
        (*TODO: UPDATE BOARD*)
        flush stdout
      done
    with End_of_file ->
      Unix.kill kidpid Sys.sigterm;
