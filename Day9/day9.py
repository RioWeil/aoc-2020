"""
Date: 12/21/2020
Name: Rio Weil
Title: day9.py
Decription: 2020 AoC D9 - Numbers in a list summing to a target value.
"""
import numpy as np

## Functions:

def twosum(n, lon):
    """
    Returns true if there exist two numbers in lon that sum to n.
    """
    filtered = [num for num in lon if num < n] # Filter out ele. larger than n.
    for i in range(len(filtered)):
        for j in range(i, len(filtered)):
            if filtered[i] + filtered[j] == n:
                return True
    return False

def contiguous_sum_sublist(n,lon):
    """
    Returns a contiguous sublist of lon whose elements sum to n.
    """
    for i in range(len(lon)):
        startindex = len(lon) - i
        result = cont_sum_from_start(n, lon, startindex)
        if (result != False):
            return result

def cont_sum_from_start(n, lon, startindex):
    """
    Returns a contiguous sublist of lon starting from startindex and
    working backwards (more time-efficient). Returns False if no such
    sublist starting from the startindex exists.
    """
    for j in range(1, startindex):
        summation = np.sum(lon[startindex-j:startindex])
        if summation > n:
            return False
        elif summation == n:
            return lon[startindex-j:startindex]
    return False

## Solution:

numlist = [int(i) for i in open('input.txt', "r").read().splitlines()]

for k in range(25, len(numlist)):
    if (twosum(numlist[k], numlist[k-25: k]) == False):
        sol1 = numlist[k]
        index = k
        break

subnumlist = numlist[:k]
contiguous_sum_list = contiguous_sum_sublist(sol1, subnumlist)
sol2 = min(contiguous_sum_list) + max(contiguous_sum_list)