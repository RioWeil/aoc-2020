"""
Date: 12/22/2020
Name: Rio Weil
Title: day13.py
Decription: 2020 AoC D13 - Modular Arithmetic and CRT
"""
import numpy as np

## Functions:
def process_input_q1(inputfile):
    """
    Processes string of form 'x,x,N1,x,...,N2,...' and produces list
    containing all of the naturals N1,....Nk.
    """
    split_input = inputfile.split(",")
    buses = []
    for bus in split_input:
        if (not bus == "x"):
            buses.append(int(bus))
    return buses

def buses_waittime(time, buslist):
    """
    From a time and a list of buses (which each arrive at an interval given
    by their number) produces list of wait times from the provided time to 
    catch the respective buses.
    """
    waittime_list = []
    for bus in buslist:
        bus_number = time // bus + 1
        wait_time = bus_number*bus - time
        waittime_list.append(wait_time)
    return waittime_list

def process_input_q2(inputfile):
    """
    Processes string of form 'x,x,N1,x,...,N2,...' and produces list of 2-len
    lists containing the natural N and the position k in the string.
    E.g. "30,x,x,12,x,10" becomes [[30,0], [12,3], [10,5]].
    """
    split_input = inputfile.split(",")
    buses = []
    for i in range(len(split_input)):
        if (not split_input[i] == "x"):
            buses.append([int(split_input[i]), i])
    return buses

def find_aligned_bustime(buses_and_spacing):
    """
    Find earliest time for which all of the buses align according to spacings.
    Note: HIGHLY INEFFICIENT - Scales exponentially and not guaranteed to halt.
    """
    t = 0
    while True:
        if aligned_time(buses_and_spacing, t):
            return t
        t = t+1

def aligned_time(buses_and_spacing, t):
    for bus_with_space in buses_and_spacing:
        bus, spacing = bus_with_space[0], bus_with_space[1]
        time_w_offset = t + spacing
        if (time_w_offset % bus != 0):
            return False
    return True

"""
The first attempt outlined above fails as the search space of integers is too large.
Instead, we consider an approach that uses the Chinese Remainder theorem, as we are
essentially solving a system of congruences.

The CRT states that given coprime positive integers n1,... nk and integers a1,...ak
if we construct a system of congruences:
    x \equiv a1 (mod n1)
    x \equiv a2 (mod n2)
    ...
    x \equiv ak (mod nk)
then there exists a solution x which is unique modulo N = n1 * n2 * ... *  nk

A method to solve the solution is given as follows:
1. Compute N = n1 * n2 * ... * nk
2. For each i = 1,2,...,k, compute:
    yi = N/ni
3. For each i = 1,2,...k, compute zi \equiv 1/yi (mod ni) using Euclid's extended algorithm.
   This can be done for solving for zi in the equation ziyi + z2ini = 1 (which is guaranteed
   to have a solution.)
4. Then, the integer x = \sum_{i=1}^{k} aiyizi is a solution to the system, and x mod N is
the unique solution mod N.
"""

def isolve(a, b, c):
    """
    Given integers a, b, c, solve for integers x, y that solve ax + by = c.
    CONSTRAINT: gcd(a,b) divides c (so, c can be 1 if a, b coprime)
    Taken from https://www.math.utah.edu/~carlson/hsp2004/PythonShortCourse.pdf
    """
    q, r = divmod(a, b)
    if r == 0:
        return([0, c/b])
    else:
        sol = isolve(b, r, c)
        u = sol[0]
        v = sol[1]
        return([v, u - q*v])

def solve_time_crt(buses_and_spacing):
    """
    Produce the solution to a system of congruences as outlined above.
    I'm not entirely sure why, but this process actually fails. Oh well.
    """
    nilist = []
    ailist = []
    for bus_and_space in buses_and_spacing:
        nilist.append(int(bus_and_space[0]))
        ailist.append(int(bus_and_space[1]))
    N = int(np.prod(nilist))
    yilist = []
    zilist = []
    for ni in nilist:
        yi = int(N/ni)
        yilist.append(yi)
        zi = int(isolve(yi, ni, 1)[0]) # Has solution as yi, ni are coprime.
        if zi < 0:
            zi = zi + ni
        zilist.append(zi)
    x = 0
    for i in range(len(ailist)):
        x = x + ailist[i] * yilist[i] * zilist[i]
    return x % N

## Solution:
inputfile = open('input.txt', "r").read().splitlines()
mytime = int(inputfile[0])
available_buses = process_input_q1(inputfile[1])
waittime_list = buses_waittime(mytime, available_buses)
min_index = waittime_list.index(min(waittime_list))
sol1 = available_buses[min_index]*waittime_list[min_index]

buses_and_spacing = process_input_q2(inputfile[1])
sol2 = solve_time_crt(buses_and_spacing)