test:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind -pkgs lymp controller.byte && ./controller.byte

repl:
	ocamlbuild -use-ocamlfind application.byte && ./application.byte

# Run bash server.sh to get IP on server host
IP = 10.131.3.56

serv:
	ocamlbuild -use-ocamlfind -pkgs lwt,lwt.unix server.byte && ./server.byte $(IP)

client_repl:
	ocamlbuild -use-ocamlfind client_repl.byte && ./client_repl.byte $(IP)

# client_gui:
# 	ocamlbuild -use-ocamlfind -pkgs lymp client_gui.byte && ./client_gui.byte $(IP)
