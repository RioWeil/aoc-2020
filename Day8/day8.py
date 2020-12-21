"""
Date: 12/21/2020
Name: Rio Weil
Title: day8.py
Decription: 2020 AoC D8 - Executing line-by-line CPU instructions
"""
import numpy as np

## Functions:

def execute_insts_onecycle(insts, pos, acc, executed):
    """
    Takes in list of instructions and executes until one cycle is completed.
    After cycle is detected, returns acc at that point.
    pos is position/instruction to execute.
    acc is accumulator.
    executed is what insts. have already been executed (list positions).
    """
    inst = insts[pos]
    inst_type, inst_sign, inst_val = inst[0:3], inst[4:5], int(inst[5:])
    if pos in executed:
        return acc
    elif inst_type == "acc":
        if inst_sign == "+":
            return execute_insts_onecycle(insts, pos + 1, acc + inst_val, executed + [pos])
        else:
            return execute_insts_onecycle(insts, pos + 1, acc - inst_val, executed + [pos])
    elif inst_type == "jmp":
        if inst_sign == "+":
            return execute_insts_onecycle(insts, pos + inst_val, acc, executed + [pos])
        else:
            return execute_insts_onecycle(insts, pos - inst_val, acc, executed + [pos])
    else:  # inst_type == "nop"
        return execute_insts_onecycle(insts, pos + 1, acc, executed + [pos])

def change_one_jmp_or_nop(instructions):
    """
    For an instructions list of size N, produces a list of N instruction sets, 
    each with a change to the instructions on the kth line; switching jmp with 
    nop (acc insts. are unchanged).
    """
    listof_listofints = []
    for pos in range(len(instructions)):
        newinsts = np.copy(instructions)
        inst = instructions[pos]
        inst_type, inst_signval = inst[0:3], inst[4:]
        if inst_type == "jmp":
            newinsts[pos] = "nop " + inst_signval
        elif inst_type == "nop":
            newinsts[pos] = "jmp " + inst_signval
        # If inst_type == "acc", does nothing/stays the same.
        listof_listofints.append(newinsts)
    return(listof_listofints)

def execute_insts_terminate(insts, pos, acc, executed):
    """
    Takes in list of instructions and executes them.
    If cycle is encountered, returns false.
    If instructions reach one line past the last line, then return acc.
    pos is position/instruction to execute.
    acc is accumulator.
    executed is what insts. have already been executed (list positions).
    """
    if pos >= len(insts):  # Reaches end of instructions
        return acc
    elif pos in executed:  # Finds a cycle
        return False
    inst = insts[pos]
    inst_type, inst_sign, inst_val = inst[0:3], inst[4:5], int(inst[5:])
    if inst_type == "acc":
        if inst_sign == "+":
            return execute_insts_terminate(insts, pos + 1, acc + inst_val, executed + [pos])
        else:
            return execute_insts_terminate(insts, pos + 1, acc - inst_val, executed + [pos])
    elif inst_type == "jmp":
        if inst_sign == "+":
            return execute_insts_terminate(insts, pos + inst_val, acc, executed + [pos])
        else:
            return execute_insts_terminate(insts, pos - inst_val, acc, executed + [pos])
    else:  # inst_type == "nop"
        return execute_insts_terminate(insts, pos + 1, acc, executed + [pos])

        
## Solution:

instructions = open('input.txt', "r").read().splitlines()
oneline_mod_instructions = change_one_jmp_or_nop(instructions)

sol1 = execute_insts_onecycle(instructions, 0, 0, [])
for insts in oneline_mod_instructions:
    result = execute_insts_terminate(insts, 0, 0, [])
    if (not result == False):
        sol2 = result

