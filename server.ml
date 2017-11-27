open Lwt

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

let users = ref 0
let outs = ref []
let current_user = ref 1

let () = Lwt_log.add_rule "*" Lwt_log.Info

let handle_message msg =
  let a = List.nth (String.split_on_char ',' msg) 0 in
  let b = List.nth (String.split_on_char ',' msg) 1 in
  if a = "text" then (true,b) else (false,"Moved " ^ a ^ " to " ^ b)

let rec broadcast l rep oc ind =
  match l with
  |[] -> []
  |(i,out)::t -> if i <> ind then
      (Lwt_io.write_line out rep;
       broadcast t rep oc ind)
      else broadcast t rep oc ind

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
            Lwt_io.write_line oc "Not enough users" >>= handle_connection ic oc ind
          else
          if is_text then
            ((broadcast !outs rep oc ind); interact)
          else
            if ind <> !current_user then
              Lwt_io.write_line oc "Not your turn" >>= handle_connection ic oc ind
            else
              if !current_user = 1 then
                ((current_user := 2);
                (broadcast !outs rep oc ind); interact)
              else
                ((current_user := 1);
                 (broadcast !outs rep oc ind); interact)
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
