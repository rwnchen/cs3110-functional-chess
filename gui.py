import Tkinter as tk
from Tkinter import *
from PIL import Image, ImageTk
import os

class GameBoard(tk.Frame):
    def __init__(self, parent, is_white=True ,rows=8, columns=8, size=75,
    color1="white", color2="grey"):
        self.rows = rows
        self.columns = columns
        self.size = size
        self.color1 = color1
        self.color2 = color2

        # Build the board
        piece_names = ["rook","knight","bishop","queen", "king", "pawn"]
        self.pieces = {}
        self.images = {}

        brow = 7 if is_white else 0
        wrow = 0 if is_white else 7
        pbrow = 6 if is_white else 1
        pwrow = 1 if is_white else 6
        # Make all the pieces
        for piece in piece_names:
            w = piece+"w"
            b = piece+"b"
            image = Image.open("img/"+w+".gif")
            image = image.resize((size, size), Image.ANTIALIAS)
            self.images[w] = ImageTk.PhotoImage(image)
            image = Image.open("img/"+b+".gif")
            image = image.resize((size, size), Image.ANTIALIAS)
            self.images[b] = ImageTk.PhotoImage(image)

            for i in range(4):
                if i != 3:
                    self.pieces[piece_names[i]+"w0"] = (i,wrow)
                    self.pieces[piece_names[i]+"b0"] = (i,brow)
                    self.pieces[piece_names[i]+"w1"] = (7-i,wrow)
                    self.pieces[piece_names[i]+"b1"] = (7-i,brow)
                else:
                    if is_white:
                        self.pieces["queenw0"] = (i,wrow)
                        self.pieces["queenb0"] = (i,brow)
                        self.pieces["kingw0"] = (7-i,wrow)
                        self.pieces["kingb0"] = (7-i,brow)
                    else:
                        self.pieces["kingw0"] = (i,wrow)
                        self.pieces["kingb0"] = (i,brow)
                        self.pieces["queenw0"] = (7-i,wrow)
                        self.pieces["queenb0"] = (7-i,brow)

                self.pieces["pawnw" + str(i)] = (i,pwrow)
                self.pieces["pawnb" + str(i)] = (i,pbrow)
                self.pieces["pawnw" + str(7-i)] = (7-i,pwrow)
                self.pieces["pawnb" + str(7-i)] = (7-i,pbrow)

        self.root = parent
        self.root.resizable(0,0) # no window resizing allowed
        self.root.title("Chess")
        self.update = False # variable to flag an update
        self.was_click = False # flag for clicks
        self.space_clicked = None # placeholder for the click type
        self.highlighted = [] # list of highlighted squares
        self.enpassant = None # piece that could be captured in en passant
        self.current_color = "w" #starting color is always white
        self.promoted = None #piece to promote
        self.popup = None #placeholder for popup window

        frame = tk.Frame.__init__(self,self.root)
        canvas_width = columns * size
        canvas_height = rows * size
        self.canvas = tk.Canvas(self.root, borderwidth=0, highlightthickness=0,
                                width=canvas_width, height=canvas_height)
        self.canvas.pack(side="left", fill="both", expand=True, padx=2, pady=2)

        # the operner window
        self.openerframe = Frame(bd=0, relief=SUNKEN,width=canvas_width)
        Label(self.openerframe,text="Openers").pack(side="top")
        self.openerframe.pack(side="top",fill="x", padx=5, pady=5)
        scrollbar = Scrollbar(self.openerframe)
        scrollbar.pack(side="right", fill="y")
        self.openerbox = Listbox(self.openerframe,
        yscrollcommand=scrollbar.set, width=60, height=10, relief="solid")
        self.openerbox.pack(side="left", fill="both")
        scrollbar.config(command=self.openerbox.yview)

        # the button window for saving and loading
        self.buttonframe = Frame(bd=1, width=canvas_width/2, height=100)
        self.buttonframe.pack(side="left",fill="x", padx=5, pady=5)
        button1 = Button(self.buttonframe, text="Load",
        command=self.load_callback())
        button2 = Button(self.buttonframe, text="Save",
        command=self.save_callback())
        button1.pack()
        button2.pack()

        # history window
        self.historyframe = Frame(bd=0, relief=SUNKEN, width=canvas_width/2)
        Label(self.historyframe,text="Move History").pack(side="top")
        self.historyframe.pack(side="right",fill="x", padx=5, pady=5)
        scrollbar = Scrollbar(self.historyframe)
        scrollbar.pack(side="right", fill="y")
        self.historybox = Listbox(self.historyframe,
        yscrollcommand=scrollbar.set, width=20, height=20, relief="solid")
        self.historybox.pack(side="right", fill="both")
        self.historybox.bind('<<ListboxSelect>>', self.get_history)

        scrollbar.config(command=self.historybox.yview)

        self.canvas.bind("<Configure>", self.refresh)

    def addpiece(self, name, column=0, row=0):
        '''Adds a piece to the playing board with [name] and places it
        at [0,0]. Then calls placepiece to move it to the [column] and [row]'''
        img = self.canvas.create_image(0,0, image=self.images[name[:-1]],
        tags=name, anchor="c")
        if (name not in self.pieces):
            self.pieces[name] = (column, row)
        self.placepiece(name, column, row)

    def placepiece(self, name, column, row):
        '''Moves a piece with [name] to the given [column] and [row]'''
        x0 = (column * self.size) + int(self.size/2)
        y0 = ((row-7) * -1 * self.size) + int(self.size/2)
        self.canvas.coords(name, x0, y0)
        self.canvas.tag_bind(name,'<ButtonPress-1>',self.callback)

    def refresh(self, event):
        '''Draw the board. Adds squares with appropriate colors to the board.
        [Event] is for window resizing, but we turned that off. '''
        xsize = int((event.width-1) / self.columns)
        ysize = int((event.height-1) / self.rows)
        self.size = min(xsize, ysize)
        self.canvas.delete("square")
        color = self.color2
        for row in range(self.rows):
            color = self.color1 if color == self.color2 else self.color2
            for col in range(self.columns):
                x1 = (col * self.size)
                y1 = ((row-7) * -1 * self.size)
                x2 = x1 + self.size
                y2 = y1 + self.size
                rect = self.canvas.create_rectangle(x1, y1, x2, y2,
                outline="black", fill=color, tags=["square",str(col)+str(row)])
                color = self.color1 if color == self.color2 else self.color2
        self.canvas.tag_bind("square",'<ButtonPress-1>',self.callback)
        for name in self.pieces:
            self.placepiece(name, self.pieces[name][0], self.pieces[name][1])
        self.canvas.tag_raise("piece")
        self.canvas.tag_lower("square")

    def highlight_rect(self, x, y):
        """adds a rectangle with center at [x],[y] that is the size of the game
        square. Colors it dark green on dark tiles and light green and white."""
        x1 = (x * self.size)
        y1 = ((y-7)*-1 * self.size)
        x2 = x1 + self.size
        y2 = y1 + self.size
        color = "lightgreen"

        if (x%2 == 0 and y%2 == 1) or (x%2 == 1 and y%2 == 0):
            color = "darkgreen"

        rect = self.canvas.create_rectangle(x1, y1, x2, y2, outline=None,
        fill=color, tags=["highlighted",str(x)+str(y)])
        self.canvas.tag_bind(rect,'<ButtonPress-1>',self.callback)

        self.highlighted.append((x,y))
        self.canvas.tag_lower("highlighted")
        self.canvas.tag_lower("square")
        self.canvas.tag_raise("piece")


    def addpieces(self):
        """ Adds all the pieces in self.pieces to the board by calling
        add piece on them. """
        for piece in self.pieces.keys():
            self.addpiece(piece, self.pieces[piece][0],self.pieces[piece][1])

    def refreshpieces(self):
        """ Moves all the pieces in self.pieces to their location that is
        defined """
        for piece in self.pieces.keys():
            self.placepiece(piece, self.pieces[piece][0],self.pieces[piece][1])

    def getpiecepos(self, piece):
        """ returns piece position of a given piece"""
        return self.pieces[piece]

    def getpieceatpos(self,pos):
        """ gets the piece with the value [pos]. Since only one piece can be on
        a square at once, each key-value pair should be unique (1-to-1)"""
        for piece,p in self.pieces.iteritems():
            if pos == p:
                return piece
        return None

    def move(self, pos1, pos2):
        """ finds the piece at [pos1], then moves it to [pos2]. Also checks for
        en passant and promotion rules, as well as casteling rules since these
        rules were hard to convey through the info we could send from ocaml."""
        piece = self.getpieceatpos(pos1)
        removed_piece = self.getpieceatpos(pos2)
        if piece is not None:
            origin = self.pieces[piece]
            dx = -(pos1[0] - pos2[0])
            dy = pos1[1] - pos2[1]
            self.canvas.move(piece, dx*self.size, dy*self.size)
            self.pieces[piece] = (pos2[0],pos2[1])

            if "king" in piece:
                # This assumes that the game logic is correct. So no checking
                # to see if castling is valid occurs
                if dx > 1:
                    rook = "rook" + piece[4] + str(1)
                    (rx,ry) = self.pieces[rook]
                    self.move((rx,ry),(rx-2,ry))
                elif dx < -1:
                    rook = "rook" + piece[4] + str(0)
                    (rx,ry) = self.pieces[rook]
                    self.move((rx,ry),(rx+3,ry))
            if "pawnb" in piece:
                (x2,y2) = pos2
                if y2 == 0:
                    self.promotion((piece,pos2))
                elif pos2 == self.enpassant:
                    removed_piece = self.getpieceatpos((x2,y2+1))

            elif "pawnw" in piece:
                (x2,y2) = pos2
                if y2 == 7:
                    self.promotion((piece,pos2))
                elif pos2 == self.enpassant:
                    removed_piece = self.getpieceatpos((x2,y2-1))
            # Clear en_passant after checking if the move was an
            # enpassant capture
            self.enpassant = None
            if "pawn" in piece:
                # Some logic for en passant
                if dy > 1:
                    self.enpassant = (pos1[0],pos1[1]-1)
                elif dy < -1:
                    self.enpassant = (pos1[0],pos1[1]+1)

            if removed_piece is not None:
                self.canvas.delete(removed_piece)
                del self.pieces[removed_piece]

            return piece
        else:
            return None

    def promotion(self,piece):
        """ Provides a pop-up window  to allow the user to promote their pawn"""
        self.popup = Toplevel()
        self.popup.title("Promotion")
        label1 = Label(self.popup, text="Choose Promotion", height=0, width=30,
        padx = 2, pady=2)
        name = "knight"+self.current_color
        piece_name = piece[0]
        piece_pos = piece[1]
        info = (piece_name,piece_pos,name)
        button1 = Button(self.popup, image=self.images[name],
        command=lambda m=info : self.promote_callback(m))
        name = "bishop"+self.current_color
        info = (piece_name,piece_pos,name)
        button2 = Button(self.popup, image=self.images[name],
        command=lambda m=info : self.promote_callback(m))
        name = "rook"+self.current_color
        info = (piece_name,piece_pos,name)
        button3 = Button(self.popup, image=self.images[name],
        command=lambda m=info : self.promote_callback(m))
        name = "queen"+self.current_color
        info = (piece_name,piece_pos,name)
        button4 = Button(self.popup, image=self.images[name],
        command=lambda m=info : self.promote_callback(m))
        label1.pack()
        button1.pack()
        button2.pack()
        button3.pack()
        button4.pack()


    def callback(self, event):
        """ The call back for most board interactions. Marks was click as true,
        and builds the data that will be sent back to Ocaml"""
        #clear highlighted squares after every click
        item = event.widget.find_withtag("current")
        tags = self.canvas.itemcget(item, "tags").split(" ")
        info = []
        if ("square" in tags):
            info = [u"empty",tags[1]]
        elif ("highlighted" in tags):
            info = [u"highlight",tags[1]]
        elif (tags[0] in self.pieces):
            pos = self.pieces[tags[0]]
            info = [u"piece",str(pos[0]) + str(pos[1])]
            if (int(pos[0]),int(pos[1])) in self.highlighted:
                info = [u"highlight",str(pos[0]) + str(pos[1])]
        else:
            info = [u"none","99"]
        # delete the highlighted squares on every click
        self.canvas.delete("highlighted")
        self.highlighted = []

        self.was_click = True
        self.clicked_space = info

    def promote(self,old_piece,pos,new_piece):
        """Promotes [old_piece] to [new_piece]. Also sets the self.promoted
        field to indicate to the backend what piece was selected. Closes popup
        when a piece is selected."""
        del self.pieces[old_piece]
        self.canvas.delete(old_piece)
        i = 0
        while (new_piece + str(i) in self.pieces):
            i += 1
        self.addpiece(new_piece+str(i),pos[0],pos[1])
        self.popup.destroy()
        self.popup = None
        self.clicked_space = ["promote",pos]
        self.promoted = new_piece[0]
        if (self.promoted == "k"):
            self.promoted = "n"
        self.was_click = True

    def promote_callback(self, p):
        """ Callback called when a pawn reaches the end of the board"""
        # Change pawn to selected piece on board
        self.promote(p[0],p[1],p[2])

    def load_callback(self):
        """ callback used when load button is clicked"""
        self.was_click = True
        self.clicked_space = ["load","testname"]

    def save_callback(self):
        """ callback used when save button is clicked"""
        self.was_click = True
        self.clicked_space = ["save",""]

    def get_history(self, event):
        """gets a single item from the play history box and buids the info
        to be sent back to the backend. """
        item = self.historybox.get(self.historybox.curselection())
        index = (int(item[0]) - self.historybox.size()+1) * -1
        self.was_click = True
        self.clicked_space = ["history",int(index)]

    def check_mate(self):
        """Opens a popup that says 'CHECKMATE'."""
        self.popup = Toplevel()
        self.popup.title("Checkmate")
        label1 = Label(self.popup, text="CHECKMATE!",
        height=0, width=0, padx = 50, pady=50)
        label1.pack()

    def stale_mate(self):
        """Opens a popup that says 'Stalemate'."""
        self.popup = Toplevel()
        self.popup.title("Stalemate")
        label1 = Label(self.popup, text="Stalemate",
        height=0, width=0, padx = 50, pady=50)
        label1.pack()

# used to keep track of move history so we can revert the gui
piece_dicts = []

# IMPORTANT: These are all functions called from the backend. So in their specs,
#   when "sends to backend", that is done just by returning a value. In many
#   cases the board is returned so we can preserve the current state of the gui.

def get_click(board):
    """Gets what the click was in the [board] and sends that data to the
    backend
        Returns: [string;[int,int]]"""
    clicked_space = list(board.clicked_space)
    click_type = clicked_space[0]
    coord_names = ["promote", "piece", "empty", "highlight"]
    if click_type == u"none":
        info = [-1,-1]
    elif click_type in coord_names:
        info = [int(clicked_space[1][0])+1,int(clicked_space[1][1])+1]
    else:
        info = clicked_space[1]
    return [unicode(click_type),info]

def get_promotion(board):
    """Gets the what piece was sleceted for promotion and send that to the
    backend.
        Returns: string"""
    promoted = board.promoted
    board.promoted = None
    return unicode(promoted)

def move(board, pos1, pos2):
    """ Calls the move function from the board module to move piece from
    [pos1] to [pos2]. Needs to correct for the 1 indexing from backend.
        Returns: Board (Pyref). """
    piece_dicts.insert(0,board.pieces.copy())
    pos1 = (pos1[0]-1,pos1[1]-1)
    pos2 = (pos2[0]-1,pos2[1]-1)
    move = board.move(pos1,pos2)
    if move is not None:
        if board.current_color == "w":
            board.current_color = "b"
        else:
            board.current_color = "w"
    return board

def highlight(board, tiles):
    """ calls the highlight function in board. Corrects for 1-indexing from
    backend.
        Returns: Board (Pyref)"""
    for x,y in tiles:
        x -= 1
        y -= 1
        if ((x,y) not in board.highlighted):
            board.highlight_rect(x,y)
    return board

def update_openers(board, s):
    """ Updates the openers. Clears the current openers and replaces it
    with the new list [s].
        Retuns: Board (Pyref)"""
    board.openerbox.delete(0,END);
    if len(s) > 0:
        for (eco, name, winrate, reply) in s:
            board.openerbox.insert(0, "{}|{}".format(eco, name))
            board.openerbox.insert(1, "White winrate: {:.3f}".format(winrate))
            board.openerbox.insert(2, "Next move: {}".format(reply[-1]))
            board.openerbox.insert(3, "-----------")
    else:
        board.openerbox.insert(0, "No openings for the current move sequence!")
    return board

def update_history(board, hist_lst):
    """ Updates the history. Clears current history and replaces it with the
    new list [hist_lst]
        Returns: Board (Pyref)"""
    # Deletes history list and updates with items in hist_lst
    board.historybox.delete(0,END);
    total_len = len(hist_lst)
    hist_lst.reverse()
    for i,h in enumerate(hist_lst):
        item = board.historybox.insert(END,
                str((i-total_len+1)*-1) + "\t\t\t" + h)
    return board

def start_game():
    """ Initializes the game and sends the starting board to the backed.
        Returns: Board (Pyref)"""
    root = tk.Tk()
    board = GameBoard(root)
    board.addpieces()
    board.pack(side="top", fill="both", expand="true", padx=4, pady=4)
    root.update_idletasks()
    root.update()
    return board

def update_game(board, update):
    """ Updates the game. This is the main game loop that  """
    board.root.update_idletasks()
    board.root.update()
    click = board.was_click
    board.was_click = False
    return click

def revert(board, i):
    """ Reverts the gui back to the state [board] is in. [i] is the index of
    the history that is stored in piece_dicts. Board is NOT the current board
    of the backend.
        Return: Board (Pyref)"""
    global piece_dicts
    # for p in board.pieces.keys():
    #     board.canvas.delete(p)
    board.pieces = {}
    board.pieces = piece_dicts[i]
    piece_dicts = piece_dicts[i+1:]
    board.refreshpieces()
    board.pack(side="top", fill="both", expand="true", padx=4, pady=4)
    return board

def check_mate_popup(board):
    """Just calls the check_mate popup function"""
    board.check_mate()
    return board

def stale_mate_popup(board):
    """Just calls the stale_mate popup function"""
    board.stale_mate()
    return board


if __name__ == "__main__":
    board = start_game()
    move(board, (2,2),(2,7))
    # move(board, (2,7),(2,1))
    stale_mate_popup(board)
    board.mainloop()
