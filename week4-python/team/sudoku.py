#!/usr/bin/env python3
import argparse
import math
import sys

NUMBERS = 0
POSITIONS = 0
BLOCKS = 0

def parse_sudoku(file):
    """ parses a sudoku from a file """

    parsed = []

    try:
        with open(file) as f:
            for line in f:
                parsed.append([item for item in parse_int(line)])
    except:
        raise ValueError(""" The sudoku file is not there, please"""
                         """ give a file to read from. """)
    size = len(parsed)

    try:
        len(parsed) == len(parsed[0])

    except:
        raise ValueError('The sudoku should be square.')

    block_size = int(math.sqrt(len(parsed)))

    # store static attributes of the sudoku as globals, for more readable code.
    global NUMBERS
    global POSITIONS
    global BLOCKS

    # define the possible numbers, the positions and blocks for the sudoku.
    NUMBERS = {i for i in range(1, size + 1)}
    POSITIONS = {i for i in range(1, size)}
    BLOCKS = [tuple((j + b1, i + b2) for i in range(block_size)
              for j in range(block_size)) for b1 in range(0, size, block_size)
              for b2 in range(0, size, block_size)]

    return parsed

def parse_int(inputstring):
    """ returns numbers in a given string as ints """

    for i in inputstring.split():
        if i == '_':
            yield 0
        elif i.isdigit():
            yield int(i)
        else:
            raise ValueError('The input could not be parsed wrong characters.')

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
        print('not done yet...\n')

    return sudoku

def fill_guaranteed(sudoku, open_spots, verbose=False):
    """ fills in guaranteed values in a sudoku,
        until no longer possible """

    filling_guarenteed = True

    while(filling_guarenteed):

        filling_guarenteed = False

        # build the set of possible positions
        possible_positions = build_possible_positions(sudoku)

        # walk over the keys in the possible positions dictionary
        for key in sorted(possible_positions,
                          key=lambda k: len(possible_positions[k]),
                          reverse=True):

            # drop filled in positions from the dictionary
            if len(possible_positions[key]) == 0:
                possible_positions.pop(key)

            # if there is only one possible value, fill it in.
            elif len(possible_positions[key]) == 1:

                # flip bool to true to retry filling in a value.
                filling_guarenteed = True

                val = possible_positions[key].pop()

                # unpack key and fill in value.
                row, col = key
                sudoku[row][col] = val

                if verbose:
                    print(f'filled in {key} with {val}.')

                break



    open_spots_current = len(possible_positions.keys())

    if verbose:
        print('\ncurrent open spots:', open_spots_current)

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
    possibles = {}

    for row, _ in enumerate(sudoku):
        for col, item in enumerate(_):
            if item == 0:
                possibles[(row, col)] = possible_per_spot(sudoku, row, col)

    return possibles

def possible_per_spot(su, row, col):
    """ return the possible values for a spot in the sudoku """

    # a value is a possible values, when it is not in the column,
    # row or block already.

    possible = {i for i in NUMBERS if not i in get_column(su, col)
                and not i in get_row(su, row)
                and not i in get_block(su, row, col)}

    return possible

def get_row(su, i):
    """ get the ith row of the sudoku """
    return su[i]

def get_column(su, i):
    """ get the ith column of the sudoku """
    return [su[j][i] for j in range(len(su))]

def get_block(m, row, col):
    """ get the block in which the col and row fall. """
    block_i = 0

    for block in BLOCKS:

        if not tuple((row, col)) in block:
            block_i += 1
        else:
            break

    block = {m[j][i] for (j, i) in BLOCKS[block_i]}

    return block

def dirty_print_sudoku(sudoku):
    """ print only the numbers """
    for item_list in sudoku:
        for item in item_list:
            print(f'{item} ', end='')
        print('')

def pretty_print_sudoku(sudoku):
    """ print some additional stuff next to only numbers """
    add_lines = 0

    if len(sudoku) == 16:
        add_lines = 1

    print('\nSolution!')
    for item_list in sudoku:
        print('-' * len(sudoku) * (4 + add_lines))
        for item in item_list:
            out = f'| {item} '
            if item < 9 and add_lines:
                out += ' '
            print(out, end="")
        print('|')
    print('-' * len(sudoku) * (4 + add_lines),'\n')

def main():

    # define an argument parser
    # consult -h for help
    parser = argparse.ArgumentParser()
    parser.add_argument('sudoku_string', action="store",
                        help='sudoku string to be parsed.')
    parser.add_argument('-verbose', action="store_true",
                        help='boolean for verbose output',
                        default=False)
    parser.add_argument('-prettyprint', action="store_true",
                        help='boolean to pretty print.',
                        default=False)

    # parse arguments
    args = parser.parse_args()
    sudoku, pretty, verbose = args.sudoku_string, args.prettyprint, args.verbose

    sudoku = parse_sudoku(sudoku)
    sudoku = solve_sudoku(sudoku, verbose)

    if pretty:
        pretty_print_sudoku(sudoku)
    else:
        dirty_print_sudoku(sudoku)

if __name__ == "__main__":
    main()
