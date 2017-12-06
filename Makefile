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

client:
	ocamlbuild -use-ocamlfind client.byte && ./client.byte 10.132.4.207 9000
