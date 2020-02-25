#!/usr/bin/env python3
import argparse
import numpy as np
import sys

NUMBERS = 0
POSITIONS = 0
BLOCKS = 0

def main():

    # define an argument parser
    # consult -h for help
    parser = argparse.ArgumentParser()
    parser.add_argument('sudoku_string', action="store",
                        help='sudoku string to be parsed')
    parser.add_argument('-verbose', action="store_true",
                        help='boolean for verbose output',
                        default=False)
    parser.add_argument('-prettyprint', action="store_true",
                        help='boolean to pretty print',
                        default=False)

    args = parser.parse_args()
    sudoku, pretty, verbose = args.sudoku_string, args.prettyprint, args.verbose

    matrix = parse_sudoku(sudoku)
    solution = solve_sudoku(matrix, verbose)

    print_sudoku(matrix, pretty)

def parse_sudoku(file):
    """ parses a sudoku from a file """
    if not file:
        raise ValueError(""" The sudoku file is not valid. """)

    parsed = []
    with open(file) as f:
        for line in f:
            parsed.append([item for item in parse_int(line)])

    try:
        len(parsed) == len(parsed[0])
    except e as e:
        raise ValueError('The sudoku should be square.')

    size = len(parsed)

    block_per_row = {4 : 2, 9 : 3, 16 : 4}

    block_size = block_per_row[size]

    global NUMBERS
    global POSITIONS
    global BLOCKS

    # define the possible numbers, the positions and blocks for the sudoku.
    NUMBERS = {i for i in range(1, size + 1)}
    POSITIONS = {i for i in range(0, size)}
    BLOCKS = [tuple((i + b1, j + b2) for i in range(block_size)
              for j in range(block_size)) for b1 in range(0, size, block_size)
              for b2 in range(0, size, block_size)]

    matrix = np.asarray(parsed)

    return matrix

def parse_int(inputstring):
    """ returns numbers in a given string as ints """
    for i in inputstring:
        if i == '_':
            yield 0
        elif i.isdigit():
            yield int(i)
        else:
            continue

def solve_sudoku(sudoku, verbose):
    """ solve a given sudoku grid, returns a solved grid """

    possible_positions = build_possible_positions(sudoku)

    open_spots = len(possible_positions.keys())

    if verbose:
        print(f'startpoint. Open spots: {open_spots}.\n')

    # fill in the certain values in the sudoku,
    # to reduce the search tree problem.
    open_spots_current, possible_positions = fill_guaranteed(sudoku, open_spots,
                                                             verbose)

    if not open_spots_current == 0 and verbose:
        print('not done yet...')

    return sudoku

def fill_guaranteed(sudoku, open_spots, verbose=False):
    """ fills in guaranteed values in a sudoku,
        until no longer possible """

    filling_guarenteed = True

    while(filling_guarenteed):

        filling_guarenteed = False

        # build the set of possible positions
        possible_positions = build_possible_positions(sudoku)

        for key in sorted(possible_positions,
                          key=lambda k: len(possible_positions[k]),
                          reverse=True):

            # if there is only one possible value, fill it in.
            if len(possible_positions[key]) == 1:
                filling_guarenteed = True

                val = possible_positions[key].pop()


                if verbose:
                    print(f'filled in {key} with {val}.')

                sudoku[key] = val

                break

            # drop empty values from the sudoku
            if len(possible_positions[key]) == 0:
                possible_positions.pop(key)

    open_spots_current = len(possible_positions.keys())

    return open_spots_current, possible_positions

# def fill_set(sudoku, possible_positions):
#     for key in possible_positions:
#         (row, col) = key
#
#         row = set()
#         for i in POSITIONS:
#             if possible_positions.get((i, row), False):
#                 possible = possible_positions[(i, row)]
#                 row.union(possible)
#
#         possible_positions[key] = {i for i in possible_positions[key]
#                                    if i not in row}

def build_possible_positions(sudoku):
    """ build a dict with (col, row): possible values from a given sudoku """
    possibles = {tuple((col,row)) : {} for col in POSITIONS for row in POSITIONS
                 if sudoku[row][col] == 0}

    for col, item_list in enumerate(sudoku):
        for row, item in enumerate(item_list):
            if item == 0:
                possibles[(col, row)] = possible_per_spot(sudoku, col, row)

    return possibles

def possible_per_spot(m, col, row):
    """ return the possible values for a spot in the sudoku """
    block_i = 0

    for block in BLOCKS:

        if tuple((col, row)) in block:
            break

        block_i += 1

    # errors here with 16 x 16 
    block = {m[b] for b in BLOCKS[block_i]}
    possible = {i for i in NUMBERS if not i in m[col, :]
                and not i in m[:, row] and not i in block}

    return possible

def print_sudoku(sudoku, pretty):
    """ print the sudoku """
    if pretty:
        pretty_print_sudoku(sudoku)
    else:
        dirty_print_sudoku(sudoku)

def dirty_print_sudoku(sudoku):
    """ print only the numbers """
    for item_list in sudoku:
        for item in item_list:
            print(f'{item} ', end='')
        print('')

    # prevent broken pipe errors
    sys.stderr.close()

def pretty_print_sudoku(sudoku):
    """ print some additional stuff next to only numbers """
    count = 0
    print('\nSolution!')
    for item_list in sudoku:
        print('-' * len(sudoku) * 4)
        for item in item_list:
            print(f'| {item} ', end="")
            if item == 0:
                count += 1
        print('|')
    print('-' * len(sudoku) * 4,'\n')
    print(f'current open spots: {count}')

    # prevent broken pipe errors
    sys.stderr.close()

if __name__ == "__main__":
    main()
