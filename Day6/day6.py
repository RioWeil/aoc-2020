"""
Date: 12/20/2020
Name: Rio Weil
Title: day6.py
Decription: 2020 AoC D6 - Counting and common letters in lists of strings
"""
import numpy as np

## Functions:
def group_answers(input_lines):
    """
    Organizes input so that answers are grouped together into sublists.
    """
    group_sublists = []
    current_sublist = []
    for answer in input_lines:
        if answer == '':
            group_sublists.append(current_sublist)
            current_sublist = []
        else:
            current_sublist.append(answer)
    group_sublists.append(current_sublist)  # For the last group
    return group_sublists

alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

def count_all_letters(low):
    """
    Returns count of all unique letters in a list of words
    """
    joined_word = "".join(elem for elem in low)
    counter = 0
    for letter in alphabet:
        if letter in joined_word:
            counter = counter + 1
    return counter


def count_matching_letters(low):
    """
    Returns the count of letters common to all words in a list of words.
    """
    letters_to_check = []
    firstw = low[0]
    counter = 0
    for i in range(len(firstw)):
        letters_to_check.append(firstw[i:i+1])
    for letter in letters_to_check:
        f = lambda word: letter in word
        if all(f(x) for x in low):
            counter = counter + 1
    return counter


## Solution:

lines = open('/Users/ryoheiweil/Desktop/Advent_of_Code/Day6/input.txt', "r").read().splitlines()
sorted_groups = group_answers(lines)

sol1 = 0
sol2 = 0
for group in sorted_groups:
    sol1 = count_all_letters(group) + sol1
    sol2 = count_matching_letters(group) + sol2