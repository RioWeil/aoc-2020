"""
Date: 12/09/2020
Name: Rio Weil
Title: day10.py
Decription: 2020 AoC D10 - Count number of valid paths in a list of integers
"""
import numpy as np

## Functions:

def plusminusn_counter(loi, n):
    # Counts the number of neighbouring elements that are n different in loi
    # CONSTRAINT: loi must be sorted
    counter = 0
    for i in range(1, len(loi)):
        if (loi[i] - loi[i-1]) == n:
            counter = counter + 1
    return counter

def split_list(loi, n):
    # Splits lists into sublists, with cutoffs where neighbours have a gap of n
    # CONSTRAINT: loi must be sorted
    sublists = []
    current_sublist = [0]
    for i in range(1, len(loi)):
        if (loi[i] - loi[i-1]) >= n:
            sublists.append(current_sublist)
            current_sublist = []
        current_sublist.append(loi[i])
    return sublists

def how_many_paths(n):
    # Count how many paths there are for lists of length n
    # Paths are valid where "jumps" between successive elements are 3 or shorter
    if n == 1 or n == 2:
        return 1
    if n == 3:
        return 2
    else:
        return how_many_paths(n-1) + how_many_paths(n-2) + how_many_paths(n-3)

## Solution

strlist = open('input.txt', "r").read().splitlines()
intlist = [int(i) for i in strlist]
intlist.append(0) # The 0 volt outlet
intlist.append(max(intlist)+3) # The joltage of the device
intlist.sort()

onev_diffs = plusminusn_counter(intlist, 1)
threev_diffs = plusminusn_counter(intlist, 3)

s1 = onev_diffs*threev_diffs

sublists = split_list(intlist, 3)
s2 = 1
for sl in sublists:
    s2 = s2 * how_many_paths(len(sl))