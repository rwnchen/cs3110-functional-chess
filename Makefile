main:
	ocamlbuild -use-ocamlfind main.byte

test:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind -pkgs lymp controller.byte && ./controller.byte

serv:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.unix server.byte && ./server.byte 10.132.4.207 9000

repl:
	ocamlbuild -use-ocamlfind application.byte && ./application.byte

client_repl:
	ocamlbuild -use-ocamlfind client_repl.byte && ./client_repl.byte 10.132.4.207 9000

client_gui:
	ocamlbuild -use-ocamlfind client_gui.byte && ./client_gui.byte 10.132.4.207 9000
