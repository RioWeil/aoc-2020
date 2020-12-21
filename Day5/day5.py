"""
Date: 12/20/2020
Name: Rio Weil
Title: day5.py
Decription: 2020 AoC D5 - Binary boarding pass IDs
"""
import numpy as np

## Functions:

def get_row_col(bpass):
    """
    Produce tuple of the row and column of the given boarding pass
    """
    return (get_row(bpass[0:7]), get_col(bpass[7:]))

def get_row(fbseq):
    """
    Produce row of boarding pass based on sequence of 8 F (front) and B (back).
    Produces integer from 0-127
    Start with all 127 seats possible.
    If first letter is F, keep 0-63, if first letter is B, keep 64-127
    Continue process with each letter, keeping either the front or back half
    of the remaining valid seats until there is only one seat remaning.
    """
    if (not fbseq):
        return 0
    elif fbseq[0] == "B":
        return(2**(len(fbseq)-1)+get_row(fbseq[1:]))
    else:
        return(get_row(fbseq[1:]))

def get_col(lrseq):
    """
    Produce col of boarding pass based on sequence of 3 R (right) and L (left).
    Produces integer from 0-7.
    Procedure is identical to what is outlined with get_row() above, with R
    keeping the upper half and L keeping the lower half.
    """
    if (not lrseq):
        return 0
    elif lrseq[0] == "R":
        return(2**(len(lrseq)-1)+get_col(lrseq[1:]))
    else:
        return(get_col(lrseq[1:]))

def get_ID(rowcol):
    """
    Produce ID of boarding pass by multiplying the row by 8 and adding the col.
    """
    return rowcol[0]*8+rowcol[1]

def missing_ID(ids, interval):
    """
    Produce the missing ID (Natural) from the ids. Missing ID will be a number
    in the interval not included in the iterator but for which the IDs greater 
    and less than 1 of the ID are included in the iterator.
    """
    for k in interval:
        if ((k-1) in ids) and ((k+1) in ids) and (k not in ids):
            return k

## Solution:

passes = open('/Users/ryoheiweil/Desktop/Advent_of_Code/Day5/input.txt', "r").read().splitlines()
passes_rowcol = map(get_row_col, passes)
passes_id = []
for i in passes_rowcol:
    passes_id.append(get_ID(i))
interval = range(8, 1016)  # Ignr IDs 0-7 1016-1023 as msng ID not at front/back

sol1 = max(passes_id)
sol2 = missing_ID(passes_id, interval)
