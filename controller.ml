open Lymp

let move_piece a b =
  failwith "Unimplemented"

let click_listener () =
  failwith "Unimplemented"

let highlight tiles =
  failwith "Unimplemented"

let openers opener_list =
  failwith "Unimplemented"

let history move_list =
  failwith "Unimplemented"

(* This is just to show we can connect and do communicate with the python file*)
let interpreter = "python2"
let py = init ~exec:interpreter "."
let gui = get_module py "gui"
let () =
  let msg = get_string gui "get_message" [Pystr ""] in
  Printf.printf "%s\n" msg;
  let msg = get_string gui "get_message" [Pystr "TEST"] in
  Printf.printf "%s\n" msg;
  (* close py *)
