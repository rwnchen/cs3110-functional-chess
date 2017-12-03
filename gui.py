import Tkinter as tk
from Tkinter import *
from PIL import Image, ImageTk
import os

class GameBoard(tk.Frame):
    def __init__(self, parent, is_white=True ,rows=8, columns=8, size=75, color1="white", color2="grey"):
        self.rows = rows
        self.columns = columns
        self.size = size
        self.color1 = color1
        self.color2 = color2

        # Build the board
        piece_names = ["rook","knight","bishop","queen", "king", "pawn"]
        self.pieces = {}
        self.images = {}

        wrow = 7 if is_white else 0
        brow = 0 if is_white else 7
        pwrow = 6 if is_white else 1
        pbrow = 1 if is_white else 6

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
        self.root.resizable(0,0)
        self.update = False
        self.was_click = False
        self.space_clicked = ""
        self.highlighted = []

        canvas_width = columns * size
        canvas_height = rows * size

        frame = tk.Frame.__init__(self,self.root)

        self.canvas = tk.Canvas(self.root, borderwidth=0, highlightthickness=0,
                                width=canvas_width, height=canvas_height)

        self.canvas.pack(side="left", fill="both", expand=True, padx=2, pady=2)

        self.openerframe = Frame(bd=0, relief=SUNKEN,width=canvas_width)
        Label(self.openerframe,text="Openers").pack(side="top")
        self.openerframe.pack(side="top",fill="x", padx=5, pady=5)

        scrollbar = Scrollbar(self.openerframe)
        scrollbar.pack(side="right", fill="y")

        listbox = Listbox(self.openerframe, yscrollcommand=scrollbar.set, width=60, height=10, relief="solid")
        for i in range(1000):
            listbox.insert(END, str(i) + "\t\tOpener Name\t\t\t\tA1-A2\t50%")
        listbox.pack(side="left", fill="both")

        scrollbar.config(command=listbox.yview)

        self.buttonframe = Frame(bd=1, relief=SUNKEN, width=canvas_width/2, height=100)
        self.buttonframe.pack(side="left",fill="x", padx=5, pady=5)
        self.historyframe = Frame(bd=0, relief=SUNKEN, width=canvas_width/2)
        Label(self.historyframe,text="Move History").pack(side="top")
        self.historyframe.pack(side="right",fill="x", padx=5, pady=5)

        scrollbar = Scrollbar(self.historyframe)
        scrollbar.pack(side="right", fill="y")

        listbox = Listbox(self.historyframe, yscrollcommand=scrollbar.set, width=20, height=20, relief="solid")
        for i in range(1000):
            listbox.insert(END, str(i) + "\t\t\t\tA1-A2")
        listbox.pack(side="right", fill="both")

        scrollbar.config(command=listbox.yview)

        self.canvas.bind("<Configure>", self.refresh)

    def addpiece(self, name, row=0, column=0):
        '''Add a piece to the playing board'''
        img = self.canvas.create_image(0,0, image=self.images[name[:-1]], tags=name, anchor="c")
        self.placepiece(name, column, row)

    def placepiece(self, name, column, row):
        '''Place a piece at the given row/column'''
        # self.pieces[name] = (row, column)
        x0 = (column * self.size) + int(self.size/2)
        y0 = (row * self.size) + int(self.size/2)
        # print self.pieces, x0,y0
        self.canvas.coords(name, x0, y0)
        self.canvas.tag_bind(name,'<ButtonPress-1>',self.callback)

    def refresh(self, event):
        '''Redraw the board, possibly in response to window being resized'''
        xsize = int((event.width-1) / self.columns)
        ysize = int((event.height-1) / self.rows)
        self.size = min(xsize, ysize)
        self.canvas.delete("square")
        color = self.color2
        for row in range(self.rows):
            color = self.color1 if color == self.color2 else self.color2
            for col in range(self.columns):
                x1 = (col * self.size)
                y1 = (row * self.size)
                x2 = x1 + self.size
                y2 = y1 + self.size
                rect = self.canvas.create_rectangle(x1, y1, x2, y2, outline="black", fill=color, tags=["square",str(col)+str(row)])
                color = self.color1 if color == self.color2 else self.color2
        self.canvas.tag_bind("square",'<ButtonPress-1>',self.callback)
        for name in self.pieces:
            self.placepiece(name, self.pieces[name][0], self.pieces[name][1])
        self.canvas.tag_raise("piece")
        self.canvas.tag_lower("square")

    def highlight_rect(self, x, y):
        """adds a rectangle with center at x,y that is the size of the game square."""
        x1 = (x * self.size)
        y1 = (y * self.size)
        x2 = x1 + self.size
        y2 = y1 + self.size
        color = "lightgreen"

        if (x%2 == 0 and y%2 == 1) or (x%2 == 1 and y%2 == 0):
            color = "darkgreen"

        rect = self.canvas.create_rectangle(x1, y1, x2, y2, outline=None, fill=color, tags=["highlighted",str(x)+str(y)])
        self.canvas.tag_bind(rect,'<ButtonPress-1>',self.callback)

        self.highlighted.append((x,y))
        self.canvas.tag_lower("highlighted")
        self.canvas.tag_lower("square")
        self.canvas.tag_raise("piece")


    def addpieces(self):
        for piece in self.pieces.keys():
            self.addpiece(piece, self.pieces[piece][0],self.pieces[piece][1])

    def getpiecepos(self, piece):
        return self.pieces[piece]

    def getpieceatpos(self,pos):
        for piece,p in self.pieces.iteritems():
            if pos == p:
                return piece
        return None

    def move(self, pos1, pos2):
        piece = self.getpieceatpos(pos1)
        if piece is not None:
            origin = self.pieces[piece]
            dx = pos2[0] - pos1[0]
            dy = pos2[1] - pos1[1]
            self.canvas.move(piece, dy*self.size, dx*self.size)
            self.pieces[piece] = (pos2[0],pos2[1])
            return piece
        else:
            return "none"

    def callback(self, event):
        #clear highlighted squars after every click
        item = event.widget.find_withtag("current")
        tags = self.canvas.itemcget(item, "tags").split(" ")
        name = tags[0]
        if ("square" in tags or "highlighted" in tags):
            name = tags[1]
        elif (name in self.pieces):
            pos = self.pieces[name]
            name = str(pos[0]) + str(pos[1])
        self.canvas.delete("highlighted")
        self.highlighted = []
        self.was_click = True
        self.clicked_space = name

def get_piece(board):
    clicked_space = list(board.clicked_space)
    return [int(clicked_space[0]),int(clicked_space[1])]

def move(board, pos1, pos2):
    board.move(pos1,pos2)
    return board

def highlight(board, x, y):
    if ((x,y) not in board.highlighted):
        board.highlight_rect(x,y)
    return board

def start_game():
    root = tk.Tk()
    board = GameBoard(root)
    board.addpieces()
    board.pack(side="top", fill="both", expand="true", padx=4, pady=4)
    root.update_idletasks()
    root.update()
    return board

def update_game(board, update):
    board.root.update_idletasks()
    board.root.update()
    click = board.was_click
    board.was_click = False
    return [board, click]


if __name__ == "__main__":
    board = start_game()
    board.mainloop()
