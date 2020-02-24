# Leo Schreuders
# student ID 5742978
# Programmeertalen

import collections


def opgave1(mylist):
    """
    Returns True if the list of length n contains all integers from 1 to n,
    otherwise False.
    """
    return sorted(mylist) == [x for x in range(1, len(mylist) + 1)]


def opgave2(mylist):
    """
    Takes a list of length n. Returns a generator that yields all integers
    in the range 1 to n that are not in the list.
    """
    listAll = range(1, len(mylist) + 1)
    for x in listAll:
        if x not in mylist:
            yield x


def opgave3a(filename):
    """
    Takes a file with integers seperated by spaces and newlines. Returns a list
    of lists of integers, each sublist corresponding to a row in the original
    file.
    """
    with open(filename) as f:
        return [[int(x) for x in line.strip(" \n").split(" ")] for line in f]


def opgave3b(mylist):
    """
    Takes a list of lists of integers, prints every sublist to a new line.
    Integers will be seperated by spaces.
    """
    for list in mylist:
        row = [str(i) for i in list]
        print(" ".join(row))


def opgave3(filename):
    """
    Takes a file with integers seperated by spaces and newlines.
    Prints this file.
    """
    opgave3b(opgave3a(filename))


def sum_nested_it(mylist):
    """
    Flattens a list of lists or iterables and returns the sum.
    """
    # Turn the list into a stack.
    stack = collections.deque([mylist])
    flat_list = collections.deque([])

    # As long as the stack is not empty, keep taking the top element.
    while stack:
        item = stack.pop()

        # If element is an iterable, put its contents on the stack.
        if isinstance(item, collections.Iterable):
            for x in item:
                stack.append(x)

        # Otherwise, add to the front of the flattened list.
        else:
            flat_list.appendleft(item)

    return sum(flat_list)
