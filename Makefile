main:
	ocamlbuild -use-ocamlfind main.byte

test:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind -pkgs lymp controller.byte && ./controller.byte

serv:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.unix server.byte && ./server.byte

repl:
	ocamlbuild -use-ocamlfind application.byte && ./application.byte
