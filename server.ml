open Lwt
open Board

(* Code based off example http://www.baturin.org/code/lwt-counter-server/ *)
let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

let users = ref 0
let outs = ref []
let current_user = ref 1
let current_color = ref White

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
  string_of_int(fst_int) ^","^ string_of_int(snd_int)

let get_command s =
  let coms = (String.split_on_char ' ' s) in
  let (p1,p2) =  ((List.nth coms 0),(List.nth coms 1)) in
  let pp1 = (String.split_on_char ',' p1) in
  let pp2 = (String.split_on_char ',' p2) in
  let pos1 = (int_of_string(List.nth pp1 0),int_of_string(List.nth pp1 1)) in
  let pos2 = (int_of_string(List.nth pp2 0),int_of_string(List.nth pp2 1)) in
  (pos1,pos2)

let rec convert_text te s ind =
  match te with
  | [] -> s
  | h::t -> if ind <> 0 then convert_text t (s^h^" ") (ind+1)
    else convert_text t s (ind+1)

let handle_message t =
  let spaces = (String.split_on_char ' ' t) in
  let (pos1,pos2) =  ((List.nth spaces 0),(List.nth spaces 1)) in
  if pos1 = "text" then (true,convert_text spaces "" 0)
  else
    (false, process_command pos1 ^ " " ^ process_command pos2)

let do_move pos1 pos2 =
  let b = !board in
  let lm = !last_move in
  let c = !current_color in
  let leg_move = legal_moves b lm c in
  match get_piece b pos1 with
  | Some p ->
    let (new_b, check) = make_move b c lm (pos1,pos2) leg_move in
    let brd = print_board new_b in
    let newlm = Some (p, (pos1,pos2)) in
    board := new_b;
    last_move := newlm;
    brd
  | None -> "No piece selected."

let rec handle_connection ic oc ind () =
  Lwt_io.read_line_opt ic >>=
     (fun msg ->
        match msg with
        | Some msg ->
          let reply = handle_message msg in
          let rep = snd reply in
          let is_text = fst reply in
          let interact = (Lwt_log.info "Interaction" >>= handle_connection ic oc ind) in
          if !users < 2 then
            (Lwt_io.write_line oc "Not enough users" >>= handle_connection ic oc ind)
          else
          if is_text then
            ((broadcast !outs rep oc ind); interact)
          else
            if ind <> !current_user then
              (Lwt_io.write_line oc "Not your turn" >>= handle_connection ic oc ind)
            else
              let (pos1,pos2) = get_command rep in
              let game = do_move pos1 pos2 in
              if game <> "No piece selected." then
                if !current_user = 1 then
                  ((current_user := 2);
                   (current_color := Black);
                   (broadcast !outs game oc 1);
                   (broadcast !outs game oc 2); interact)
                else
                  ((current_user := 1);
                   (current_color := White);
                   (broadcast !outs game oc 1);
                   (broadcast !outs game oc 2); interact)
              else ((broadcast !outs game oc ind); interact)
        | None ->
          (* users := !users - 1; *)
          Lwt_log.info "Connection closed" >>= return)

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
