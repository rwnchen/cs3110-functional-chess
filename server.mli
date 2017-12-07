open Lwt

type pos = int * int

type move = pos * pos

type mess =
  |TextMes of string
  |PosMes of move
  |Hist of string

val broadcast : ('a * Lwt_io.output_channel) list -> string -> 'b -> 'a -> 'c list

val handle_message : string -> mess

val handle_connection : Lwt_io.input_channel -> Lwt_io.output_channel -> int -> unit -> 'a Lwt.t

val accept_connection : Lwt_unix.file_descr * 'a -> unit t

val create_socket : unit -> Lwt_unix.file_descr

val get_out : Lwt_unix.file_descr * 'a -> unit t

val create_server : Lwt_unix.file_descr -> unit -> 'a t
