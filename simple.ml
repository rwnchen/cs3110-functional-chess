open Lymp

(* change "python3" to the name of your interpreter *)
let interpreter = "python2"
let py = init ~exec:interpreter "."
let simple = get_module py "simple"

let () =
  (* msg = simple.get_message() *)
  print_endline "Test0...";
  let msg = get_string simple "get_message" [] in
  print_endline "Test1...";
  let integer = get_int simple "get_integer" [] in
  print_endline "Test2...";
  let addition = get_int simple "sum" [Pyint 12 ; Pyint 10] in
  print_endline "Test3...";
	let strconcat = get_string simple "sum" [Pystr "first " ; Pystr "second"] in
	Printf.printf "%s\n%d\n%d\n%s\n" msg integer addition strconcat ;

	close py
