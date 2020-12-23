"""
Date: 12/22/2020
Name: Rio Weil
Title: day11.py
Decription: 2020 AoC D11 - Cellular Automaton
"""
import numpy as np

## Data definitions:
"""
Board is 2D Array of Integers
interp. is a map of the seats in the waiting area
0 represents the seat is empty
1 represents the seat is occupied
2 represents a floor space
"""
example_board_1 = np.array([[1,2], [1,0]])
example_board_2 = np.array([[1,2,0],[0,0,0],[1,2,1]])

## Functions:

def initialize(inputlist):
    """
    Produces corresponding initial board from the provided input.
    empty (L) becomes 0, occupied (#) becomes 1, and floor (.) becomes 2
    """
    ydim, xdim = len(inputlist), len(inputlist[0])
    board = np.zeros((ydim,xdim))
    for y in range(ydim):
        for x in range(xdim):
            if inputlist[y][x:x+1] == "L":
                board[y,x] = 0
            elif inputlist[y][x:x+1] == "#":
                board[y,x] = 1
            else:  # inputlist[y][x:x+1] == ".":
                board[y,x] = 2
    return board

def update_board(board, birthrule, deathrule):
    """
    Produce board updated after one iteration according to cell neighbours rules.
    birthrule is number of neighbours that have to be occupied (1) for empty (0) cell
    to become occupied (1)
    deathrule is minimum number of neighbours that have to be occupied (1) for
    an occupied cell (1) to go from occupied (1) to empty (0).
    Otherwise, cells remain unchanged.
    """
    newboard = np.copy(board)
    (ydim, xdim) = board.shape
    for y in range(ydim):
        for x in range(xdim):
            newboard[y,x] = update_cell(board[y,x], get_neighbours(board, y, x, ydim-1, xdim-1), birthrule, deathrule)
    return newboard

def update_board_2(board, birthrule, deathrule):
    """
    Produce board updated after one iteration according to visible cell rules.
    birthrule is number of visible seats that have to be occupied (1) for empty (0) cell
    to become occupied (1)
    deathrule is minimum number of visible seats that have to be occupied (1) for
    an occupied cell (1) to go from occupied (1) to empty (0).
    Otherwise, cells remain unchanged.
    """
    newboard = np.copy(board)
    (ydim, xdim) = board.shape
    for y in range(ydim):
        for x in range(xdim):
            newboard[y,x] = update_cell(board[y,x], get_visible(board, y, x), birthrule, deathrule)
    return newboard

def get_neighbours(board, y, x, ydim, xdim):
    """
    Get list of vals of neighbouring 8 cells of the y,x cell on the board.
    """
    if y == 0:
        if x == 0:
            return [board[1, 0], board[0, 1], board[1, 1]]
        elif x == xdim:
            return [board[1, xdim], board[0, xdim-1], board[1, xdim-1]]
        else:
            return [board[0, x-1], board[1, x-1], board[1, x], board[1, x+1], board[0, x+1]]
    elif y == ydim:
        if x == 0:
            return [board[ydim, 1], board[ydim-1, 0], board[ydim-1, 1]]
        elif x == xdim:
            return [board[ydim, xdim-1], board[ydim-1, xdim], board[ydim-1, xdim-1]]
        else:
            return [board[ydim, x-1], board[ydim-1, x-1], board[ydim-1, x], board[ydim-1, x+1], board[ydim, x+1]]
    else:
        if x == 0:
            return [board[y+1, 0], board[y+1, 1], board[y, 1], board[y-1, 1], board[y-1, 0]]
        elif x == xdim:
            return [board[y+1, xdim], board[y+1, xdim-1], board[y, xdim-1], board[y-1, xdim-1], board[y-1, xdim]]
        else:
            return [board[y+1, x+1], board[y+1, x], board[y+1, x-1], board[y, x-1], board[y-1, x-1], board[y-1, x], board[y-1,  x+1], board[y, x+1]]

def get_visible(board, y, x):
    """
    Produce an 8 element long list of occupied/non occupied seats visible from the given position.
    An element of 0 corresponds to no visible occupied seat in that direction.
    An element of 1 corresponds to a visible occupied seat in that direction.
    Floor spaces (2) get looked/skipped over. A wall (the edge of the array)
    counts as a 0, as does an empty seat. 
    """
    visible_list = []
    visible_list.append(visible(board, y, x, 1, 1))
    visible_list.append(visible(board, y, x, 1, 0))
    visible_list.append(visible(board, y, x, 1, -1))
    visible_list.append(visible(board, y, x, 0, 1))
    visible_list.append(visible(board, y, x, 0, -1))
    visible_list.append(visible(board, y, x, -1, 1))
    visible_list.append(visible(board, y, x, -1, 0))
    visible_list.append(visible(board, y, x, -1, -1))
    return visible_list

def visible(board, y, x, ydir, xdir):
    """
    Determine if there is a visible occupied seat travelling in the specific x-y direction.
    Goes from the cell by incrementing the position by ydir, xdir until either an occupied,
    empty seat, or the wall of the array is encountered. Returns 1 in the first case, 0
    in the latter two cases.
    """
    ydim, xdim = board.shape
    newy, newx = y + ydir, x + xdir
    if (newy < 0) or (newy >= ydim) or (newx < 0) or (newx >= xdim):
        return 0
    elif board[newy, newx] == 0:
        return 0
    elif board[newy, newx] == 1:
        return 1
    elif board[newy, newx] == 2:
        return visible(board, newy, newx, ydir, xdir)


def update_cell(cell, neighbours, birthrule, deathrule):
    """
    Produces value of cell on board in next generation.
    cell is Integer, current value of the cell.
    neighbours is a list of Integers of current neighbour values.
    birthrule is number of neighbours that have to be occupied for the cell
    to become occupied (if it is empty)
    deathrule is minimum number of neighbours that have to be occupied for
    the cell to go from occupied to empty.
    """
    if cell == 0:
        occupied_nbs = count_occupied_list(neighbours)
        if occupied_nbs == birthrule:
            return 1
        else:
            return 0
    elif cell == 1:
        occupied_nbs = count_occupied_list(neighbours)
        if occupied_nbs >= deathrule:
            return 0
        else:
            return 1
    else:  # cell == 2:
        return 2

def arrays_equal(arr1, arr2):
    """
    Produce true if two provided arrays are equal.
    """
    comparison = arr1 == arr2
    equality = comparison.all()
    return equality

def count_occupied_list(celllist):
    """
    Count number of 1s in list.
    """
    counter = 0
    for val in celllist:
        counter = counter + (val % 2)
    return counter
    

def count_occupied_array(board):
    """
    Count number of 1s in board.
    """ 
    counter = 0
    (ydim, xdim) = board.shape
    for y in range(ydim):
        for x in range(xdim):
            counter = counter + (board[y,x] % 2)
    return counter


## Solution:

inputlist = open('input.txt', "r").read().splitlines()

board = initialize(inputlist)
board2 = np.zeros(board.shape)
while (not arrays_equal(board, board2)):
    board2 = np.copy(board)
    board = update_board(board2, 0, 4)
sol1 = count_occupied_array(board)

board = initialize(inputlist)
board2 = np.zeros(board.shape)
while (not arrays_equal(board, board2)):
    board2 = np.copy(board)
    board = update_board_2(board2, 0, 5)
sol2 = count_occupied_array(board)
print(sol2)