open Lwt
open Board

(* Code based off example http://www.baturin.org/code/lwt-counter-server/ *)
type pos = int * int

type move = pos * pos

type mess =
  |TextMes of string
  |PosMes of move
  |Hist of string * int option

let listen_address =
  if Array.length Sys.argv < 2
    then
      (Printf.printf "usage : server port\n"; exit 2)
    else
      Unix.inet_addr_of_string (Sys.argv.(1))

let port = 9000

let backlog = 10

let users = ref 0
let outs = ref []
let current_user = ref 1
let current_color = ref White
let hist_string = ref ""
let hist_data = ref []
let moves = ref 0

let last_move = ref None

let board = ref init_board

let () = Lwt_log.add_rule "*" Lwt_log.Info

let rec broadcast l rep oc ind =
  match l with
  |[] -> []
  |(i,out)::t -> if i <> ind then
      (Lwt_io.write_line out rep;
       broadcast t rep oc ind)
    else broadcast t rep oc ind

let process_command s =
  let fst_int = Char.code (String.get s 0) - 64 in
  let snd_int = int_of_char (String.get s 1) - 48 in
  (fst_int,snd_int)

let rec convert_text te s ind =
  match te with
  | [] -> s
  | h::t -> if ind <> 0 then convert_text t (s^h^" ") (ind+1)
    else convert_text t s (ind+1)

let handle_message t =
  let spaces = (String.split_on_char ' ' t) in
  if  (List.nth spaces 0) = "history" then
    if (List.length spaces) < 2 then Hist ("history",None)
    else
        let num = List.nth spaces 1 in
        let n = int_of_char (String.get num 0) - 48 in
        Hist ("history", Some(n))
  else
  if List.length spaces < 2 then
    TextMes "bad input"
  else
  let (pos1,pos2) =  ((List.nth spaces 0),(List.nth spaces 1)) in
  if pos1 = "text" then TextMes (convert_text spaces "" 0)
  else
    PosMes (process_command pos1,process_command pos2)

let do_move pos1 pos2 =
  let b = !board in
  let lm = !last_move in
  let c = !current_color in
  let leg_move = legal_moves b lm c in
  match get_piece b pos1 with
  | Some p ->
    let (new_b, check) = make_move b c lm (pos1,pos2) leg_move in
    if b = new_b then "Invalid move." else
    let brd = print_board new_b in
    let newlm = Some (p, (pos1,pos2)) in
    board := new_b;
    last_move := newlm;
    brd
  | None -> "No piece selected."

let rec kill_all l u =
  match l with
  |[] -> []
  |(i,out)::t ->
    (Lwt_io.fprintf out "Other player quit! \n" >>= fun () -> Lwt_io.close out);
    kill_all t u

let get_hist n =
  let (b,u) = List.assoc n !hist_data in
  board := b;
  current_user := u;
  moves := n;
  let brd = print_board !board in
  brd

let rec handle_connection ic oc ind () =
  Lwt_io.read_line_opt ic >>=
     (fun msg ->
        match msg with
        | Some msg ->
          let reply = handle_message msg in
          let interact = (Lwt_log.info "Interaction" >>= handle_connection ic oc ind) in
          if !users < 2 then
            (Lwt_log.info "Not enough users" >>= handle_connection ic oc ind)
          else
          begin
            match reply with
            |Hist (t,o) ->
              begin
                match o with
                  |Some n ->
                    if !hist_data = [] then (Lwt_io.write_line oc "No history" >>= handle_connection ic oc ind)
                    else
                      let b = get_hist n in
                      broadcast !outs b oc 1;
                      broadcast !outs b oc 2;
                      interact
                  |None ->
                    (Lwt_io.write_line oc !hist_string >>= handle_connection ic oc ind)
                end
            |TextMes rep ->
              ((broadcast !outs rep oc ind); interact)
            |PosMes (pos1,pos2) ->
            if ind <> !current_user then
              (Lwt_log.info "Not your turn" >>= handle_connection ic oc ind)
            else
              let game = do_move pos1 pos2 in
              broadcast !outs game oc 1;
              broadcast !outs game oc 2;
              (if game <> "No piece selected." && game <> "Invalid move." then
                 hist_string := !hist_string^"\n"^msg;
                 moves := !moves + 1;
                 hist_data := !hist_data@[(!moves,(!board,!current_user))];

                if !current_user = 1 then
                    begin
                      current_user := 2;
                      current_color := Black;
                    end
                else
                  begin
                    current_user := 1;
                    current_color := White;
                end);
              interact
          end
        | None ->
          kill_all !outs ind;
          Lwt_log.info "Connection closed" >>= return;
          exit 0;)

let accept_connection conn =
  users := !users + 1;
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  outs := (!users, oc)::!outs;
  let run = (Lwt.on_failure (handle_connection ic oc !users ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
              Lwt_log.info "New connection" >>= return) in run

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(listen_address, port);
  listen sock backlog;
  sock

let get_out conn =
  let fd, _ = conn in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt_io.fprintf oc "Too Many Users \n" >>= fun () ->
  Lwt_io.close oc

let create_server sock =
  let rec serve () =
    if !users < 2 then
        Lwt_unix.accept sock >>= accept_connection >>= serve
      else
        Lwt_unix.accept sock >>= get_out >>= serve
  in serve

let () =
  let sock = create_socket () in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()
