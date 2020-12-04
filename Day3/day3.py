"""
Date: 12/02/2020
Name: Rio Weil
Title: day3.py
Decription: 2020 AoC D3 - Count #s in grid when traversed with specified slope.
"""
import numpy as np

## Functions:

def tree_number(xslope, yslope, landscape):
    xpos = 0
    counter = 0
    line_length = len(landscape[0])
    for i in range(len(landscape)):
        string_index = xpos % line_length
        if (i % yslope == 0):
            if landscape[i][string_index] == '#':
                counter = counter + 1
            xpos = xpos + xslope
    return counter

## Solution

landscape = open('input.txt', "r").read().splitlines()

N11 = tree_number(1, 1, landscape)
N31 = tree_number(3, 1, landscape)
N51 = tree_number(5, 1, landscape)
N71 = tree_number(7, 1, landscape)
N12 = tree_number(1, 2, landscape)

sol1 = N31
sol2 = N11 * N31 * N51 * N71 * N12