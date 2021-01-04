"""
Date: 12/24/2020
Name: Rio Weil
Title: day15.py
Decription: 2020 AoC D15 - Numbers, Lists, and Memory Games
"""
import numpy as np

## Functions:

def play_game1(input_list, niter):
    """
    Plays game and returns list with the first niter numbers
    """
    game_history = input_list.copy()
    game_history.reverse()
    counter = 1
    for i in range(len(game_history), niter):
        counter = counter + 1
        print(counter)
        if game_history[0] in game_history[1:]:
            game_history.insert(0, (game_history[1:].index(game_history[0])+1))
        else:
            game_history.insert(0,0)
    game_history.reverse()
    return game_history

def play_game2(input_list, niter):
    """
    Returns list where the position represents the number, and the value stored there
    is the age of the number. The niter-th number in the game can then be found by
    searching the resulting final list for the index of niter.
    """
    game_history = [0]*niter  # Initialize a list of niter 0s
    counter = 1
    for vals in input_list:  # Loads in the initial values into the game history.
        game_history[vals] = counter
        counter = counter + 1 
    age = 0        # Assumes no repeats in the initial values, so initial age is 0
    for pos in range(len(input_list)+1, niter+1):
        if game_history[age] == 0:
            newage = 0  # If number has not been seen before, age is zero
            game_history[age] = pos  # Record current age
            age = newage
            pos = pos + 1  # Incremenet age
        else:
            newage = pos - game_history[age]  # If number has been seen, age is current minus last seen.
            game_history[age] = pos
            age = newage
            pos = pos + 1
    return(game_history)
            

## Solution:
input_list = [5,2,8,16,18,0,1]

result1 = play_game1(input_list, 2020)
sol1 = result1[-1]
print(sol1)

result2 = play_game2(input_list, 30000000)
sol2 = result2.index(30000000)
print(sol2)
