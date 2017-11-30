main:
	ocamlbuild -use-ocamlfind main.byte

test:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind -pkgs lymp controller.byte && ./controller.byte

serv:
	ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o server ./server.ml && ./server
