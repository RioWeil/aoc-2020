"""
Date: 11/30/2020
Name: Rio Weil
Title: day1.py
Decription: 2020 AoC D1 - Find product of 2/3 numbers in input that sum to 2020
"""
import numpy as np

## Functions:

def two_sumto_2020(numlist):
	"""
	Returns two numbers in list that sum to 2020.
	"""
	length = len(numlist)

	for i in range(length):
		for j in range(i, length):
			n1 = numlist[i]
			n2 = numlist[j]
			if n1 + n2 == 2020:
				return(n1, n2)
	return("No two numbers in the list sum to 2020.")


def three_sumto_2020(numlist):
	"""
	Returns three numbers in list that sum to 2020.
	"""
	length = len(numlist)

	for i in range(length):
		for j in range(i, length):
			for k in range(j, length):
				n1 = numlist[i]
				n2 = numlist[j]
				n3 = numlist[k]
				if n1 + n2 + n3 == 2020:
					return(n1, n2, n3)
	return("No three numbers in the list sum to 2020.")

## Solution:

input_numbers = np.genfromtxt(fname='input.txt')

sol1 = np.prod(two_sumto_2020(input_numbers))
sol2 = np.prod(three_sumto_2020(input_numbers))