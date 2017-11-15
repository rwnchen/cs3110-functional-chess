(* Redeclared because ocaml. *)
type tag_pair =
  | Event of string
  | Site of string * string * string
  | Date of int * int * int  (* Year * Month * Day *)
  | Round of int
  | White of string * string (* Lastname * Firstname *)
  | Black of string * string
  | Result of string         (* TODO: Consider a new type for game state*)
  | Tag of string * string   (* tag name * tag contents *)

type replay =
  {
    tags : tag_pair list;  (* The tag pairs *)
    moves : string array;  (* The set of moves *)
    nmove : int;           (* The next move to make *)
  }

let placeholder = { tags = [];
                    moves = [||];
                    nmove = 0; }

(* Helper functions *)


(* Exposed module functions
 * TODO EVERYTHING *)
let load_pgn file = []

let forward r = r, "d3"

let backward r = r, "d3"

let tags r = r.tags

let save_pgn r f = ()
