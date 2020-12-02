"""
Date: 12/01/2020
Name: Rio Weil
Title: day2.py
Decription: 2020 AoC D2 - Count # of passwords that meet specified conditions.
"""

## Functions:

def info_divide(s):
    """
    Takes string of the form "N1-N2 char: password\n"
    Produces N1 (Natural), N2 (Natural), char (String), password (String) 
    """
    nums, charcolon, password = s.split()
    N1, dash, N2 = nums.partition('-')
    char = charcolon[0]
    return int(N1), int(N2), char, password

def valid_password_p1(lb, ub, char, password):
    """
    Return 1 if password is valid (contains char LB <= x <= UB times), 0 if not
    """
    count = password.count(char)
    if lb <= count <= ub:
        return 1
    else:
        return 0
    
def valid_password_p2(pos1, pos2, char, password):
    """
    Return 1 if password is valid (contains char at one of pos1/pos2), 0 if not
    """
    pos1, pos2 = pos1 - 1, pos2 - 1  # Accounting for shift to 0 based indexing
    if (password[pos1] == char) != (password[pos2] == char):
        return 1
    else:
        return 0

## Solution

text_file = open('/Users/ryoheiweil/Desktop/Advent_of_Code/Day2/input.txt', "r")
lines = text_file.readlines()
text_file.close()

counter_p1 = 0
counter_p2 = 0

for line in lines:
    N1, N2, char, password = info_divide(line)
    counter_p1 = counter_p1 + valid_password_p1(N1, N2, char, password)
    counter_p2 = counter_p2 + valid_password_p2(N1, N2, char, password)

sol1 = counter_p1
sol2 = counter_p2