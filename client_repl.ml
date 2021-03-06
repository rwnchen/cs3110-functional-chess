(*Largely borrrows from http://pleac.sourceforge.net/pleac_ocaml/sockets.html*)
(* Equivalent to running 'telnet ip port'*)

let listen_address =
  if Array.length Sys.argv < 2
  then
    (Printf.printf "usage : server port\n"; exit 2)
  else
    Unix.inet_addr_of_string (Sys.argv.(1))

let port = 9000

let sockaddr = Unix.ADDR_INET (listen_address, port)

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket sockaddr;
  match Unix.fork () with
  | 0 ->
    let output = Unix.out_channel_of_descr socket in
    while true do
      let line = input_line stdin in
      output_string output line;
      output_string output "\n";
      flush output
    done
  | kidpid ->
    let input = Unix.in_channel_of_descr socket in
    try
      while true do
        let line = input_line input in
        output_string stdout line;
        output_string stdout "\n";
        flush stdout
      done
    with End_of_file ->
      Unix.kill kidpid Sys.sigterm

let () = exit 0
