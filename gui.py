import Tkinter as tk
from Tkinter import Frame
import os

class GameBoard(tk.Frame):
    def __init__(self, parent, is_white=True ,rows=8, columns=8, size=75, color1="white", color2="grey"):
        '''size is the size of a square, in pixels'''

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
            self.images[w] = tk.PhotoImage(file="img/"+w+".gif")
            self.images[b] = tk.PhotoImage(file="img/"+b+".gif")

            for i in range(4):
                self.pieces[piece_names[i]+"w0"] = (wrow,i)
                self.pieces[piece_names[i]+"b0"] = (brow,i)
                self.pieces[piece_names[i]+"w1"] = (wrow,7-i)
                self.pieces[piece_names[i]+"b1"] = (brow,7-i)

                self.pieces["pawnw" + str(i)] = (pwrow,i)
                self.pieces["pawnb" + str(i)] = (pbrow,i)
                self.pieces["pawnw" + str(7-i)] = (pwrow,7-i)
                self.pieces["pawnb" + str(7-i)] = (pbrow,7-i)

        self.root = parent
        self.update = False
        self.was_click = False
        self.space_clicked = ""
        self.highlighted = []

        canvas_width = columns * size
        canvas_height = rows * size

        tk.Frame.__init__(self, parent)

        self.canvas = tk.Canvas(self, borderwidth=0, highlightthickness=0,
                                width=canvas_width, height=canvas_height, background="bisque")
        self.canvas.pack(side="top", fill="both", expand=True, padx=2, pady=2)

        # this binding will cause a refresh if the user interactively
        # changes the window size
        self.canvas.bind("<Configure>", self.refresh)

    def addpiece(self, name, row=0, column=0):
        '''Add a piece to the playing board'''
        img = self.canvas.create_image(0,0, image=self.images[name[:-1]], tags=name, anchor="c")
        self.placepiece(name, row, column)

    def placepiece(self, name, row, column):
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
    return unicode(board.clicked_space)

def move(board, pos1, pos2):
    return unicode(board.move(pos1,pos2))

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
    while (update_game(board,True)):
        print "Updated."
