# cs3110_final

# How to Launch Game Modes

## GUI
- GUI requires:
  - ocaml Lymp module (opam install lymp)
  - Python 2.7.x (we're running 2.7.3 and did not test with other versions)
  - Python packages Tkinter and PIL
    - Tkinter install on mac: brew install homebrew/dupes/tcl-tk (you need homebrew. We don't know how to install it otherwise easily)
    - Tkinter on linux: sudo apt-get install python python-tk
    - PIL: pip install PIL
  - Lymp also requires python module bson, but new versions of bson do not work. The easiest way to get bson is to install pymongo (pip install pymongo). You also could install bson manually (pip install bson=0.4.3), but you may run into issues with this.

- To run the gui: make run


## Network

- Make sure to install Lwt
- On one computer launch the server by first running bash server.sh to get the local IP
- Then replace the IP variable in the make file
- Run "make serv" to launch the server
- On two computers of choice (either the same or different) run "make client" to launch the players
- The computers generally have to be in the same room to work together because the server is not being hosted online
- Enjoy chess by picking moves by typing for example "A2 A3" to move a pawn
- Communicate with your partner by typing first "text" then a message, ie "text hi"
- View the game history by typing "history"
- Or go back in time by typing "history" then a move number, ie "history 5"
