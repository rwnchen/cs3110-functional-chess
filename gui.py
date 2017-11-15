import Tkinter as tk
import os

class GameBoard(tk.Frame):
    def __init__(self, parent, rows=8, columns=8, size=75, color1="white", color2="grey"):
        '''size is the size of a square, in pixels'''

        self.rows = rows
        self.columns = columns
        self.size = size
        self.color1 = color1
        self.color2 = color2
        self.pieces = {
        "rookw1":(7,0),
        "rookb1":(0,0),
        "rookw2":(7,7),
        "rookb2":(0,7)
        }
        self.images = {
        "rookb":tk.PhotoImage(file="img/rookb.gif"),
        "rookw":tk.PhotoImage(file="img/rookw.gif")
        }

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
        self.canvas.create_image(0,0, image=self.images[name[:-1]], tags=(name, "piece"), anchor="c")
        self.placepiece(name, row, column)

    def placepiece(self, name, row, column):
        '''Place a piece at the given row/column'''
        self.pieces[name] = (row, column)
        x0 = (column * self.size) + int(self.size/2)
        y0 = (row * self.size) + int(self.size/2)
        # print self.pieces, x0,y0
        self.canvas.coords(name, x0, y0)

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
                self.canvas.create_rectangle(x1, y1, x2, y2, outline="black", fill=color, tags="square")
                color = self.color1 if color == self.color2 else self.color2
        for name in self.pieces:
            self.placepiece(name, self.pieces[name][0], self.pieces[name][1])
        self.canvas.tag_raise("piece")
        self.canvas.tag_lower("square")

    def addpieces(self):
        self.addpiece("rookw1", self.pieces["rookw1"][0],board.pieces["rookw1"][1])
        self.addpiece("rookb1", self.pieces["rookb1"][0],board.pieces["rookb1"][1])
        self.addpiece("rookw2", self.pieces["rookw2"][0],board.pieces["rookw2"][1])
        self.addpiece("rookb2", self.pieces["rookb2"][0],board.pieces["rookb2"][1])

def get_message(s):
    return u"Test: " + s

if __name__ == "__main__":
    root = tk.Tk()
    board = GameBoard(root)
    board.addpieces()
    board.pack(side="top", fill="both", expand="true", padx=4, pady=4)
    x = ""
    while x=="":
        root.update_idletasks()
        root.update()
        x = raw_input("TEST: ")
