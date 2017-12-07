open Lwt

type pos = int * int

type move = pos * pos

type mess =
  |TextMes of string
  |PosMes of move
  |Hist of string * int option

(*[broadcast] Broadcasts text to desired output*)
val broadcast : ('a * Lwt_io.output_channel) list -> string -> 'b -> 'a -> 'c list

(*[handle_message] Handles input and converts from string into either Text, Hist, or Positions*)
val handle_message : string -> mess

(*[handle_connection] takes input and output and applies game logic*)
val handle_connection : Lwt_io.input_channel -> Lwt_io.output_channel -> int -> unit -> 'a Lwt.t

(*[accept_connection] creates a new user in the network*)
val accept_connection : Lwt_unix.file_descr * 'a -> unit t

(*[create_socket] builds a new socket for a user*)
val create_socket : unit -> Lwt_unix.file_descr

(*[get_out] kills a process if there are too many users*)
val get_out : Lwt_unix.file_descr * 'a -> unit t

(*[create_server] adds a sock to the network or kills it if there are too many users*)
val create_server : Lwt_unix.file_descr -> unit -> 'a t
