main:
	ocamlbuild -use-ocamlfind main.byte

test:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind -pkgs lymp controller.byte && ./controller.byte

repl:
	ocamlbuild -use-ocamlfind application.byte && ./application.byte

IP = 10.132.4.207
PORT = 9000

serv:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.unix server.byte && ./server.byte $(IP) $(PORT)

client_repl:
	ocamlbuild -use-ocamlfind client_repl.byte && ./client_repl.byte $(IP) $(PORT)

client_gui:
	ocamlbuild -use-ocamlfind client_gui.byte && ./client_gui.byte $(IP) $(PORT)
