:- begin_tests(individueel).

% Shows different ways of testing in prolog.

test(myappend) :-
    individueel:append([a, b], [c], [a, b, c]).


test(myappend_assert) :-
    assertion(individueel:append([a], [b], [a, b])),
    assertion(individueel:append([2, 3], [1, 4], [2, 3, 1, 4])),
    assertion(individueel:append(["Hello "], ["World"], ["Hello ", "World"])),
    assertion(individueel:append([1, 2, 3, 4, 5], [6, 7, 8, 9, 10],
                                 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])).


test(palindroom) :-
    individueel:palindroom([1, 2, 1]).


test(palindroom_false, fail) :-
    individueel:palindroom([1, 2, 2]).


test(palindroom_false_assert) :-
    assertion(not(individueel:palindroom([a, b, c]))),
    assertion(not(individueel:palindroom([100, 200]))),
    assertion(not(individueel:palindroom([1, 2, 3]))),
    assertion(not(individueel:palindroom([1, 2, 3, 4, 5, 6, 7, 8]))),
    assertion(not(individueel:palindroom(['a', 'b']))),
    assertion(not(individueel:palindroom([individueel:palindroom/1,
                                          individueel:append/3]))).


test(palindroom_assert) :-
    assertion(individueel:palindroom([])),
    assertion(individueel:palindroom([1])),
    assertion(individueel:palindroom([a, a, a])),
    assertion(individueel:palindroom([1, 2, 2, 1])),
    assertion(individueel:palindroom([5, 5, 5, 5, 5, 5, 5])),
    assertion(individueel:palindroom(['a', 'a'])),
    assertion(individueel:palindroom([individueel:palindroom/1,
                                      individueel:palindroom/1])).


test(sudoku9_1, [nondet]) :-
    individueel:sudoku9(
        [ [5,3,4,6,7,8,9,1,_]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,_,3,4,2,5,6,7]
        , [8,5,9,7,6,_,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [_,4,5,2,_,6,1,_,9]],

        [ [5,3,4,6,7,8,9,1,2]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,8,3,4,2,5,6,7]
        , [8,5,9,7,6,1,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [3,4,5,2,8,6,1,7,9]]).


test(sudoku9_2, [nondet]) :-
    individueel:sudoku9(
        [ [5,3,4,6,7,8,9,1,_]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,_,3,4,2,5,6,7]
        , [8,5,9,7,6,_,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [_,4,5,2,_,6,1,_,9]],

        [ [5,3,4,6,7,8,9,1,2]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,8,3,4,2,5,6,7]
        , [8,5,9,7,6,1,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [3,4,5,2,8,6,1,7,9]]).


test(sudoku9_3, [nondet]) :-
    individueel:sudoku9(
        [ [_,3,7,8,1,6,2,4,_]
        , [9,6,1,4,7,2,5,8,3]
        , [8,4,2,3,5,_,7,1,6]
        , [7,5,3,6,8,1,4,9,2]
        , [1,9,6,_,4,7,8,3,5]
        , [4,_,8,9,3,5,1,6,7]
        , [3,_,5,_,9,4,6,_,_]
        , [6,1,9,5,_,8,3,7,4]
        , [_,8,4,7,_,3,9,5,1]],

        [ [5,3,7,8,1,6,2,4,9]
        , [9,6,1,4,7,2,5,8,3]
        , [8,4,2,3,5,9,7,1,6]
        , [7,5,3,6,8,1,4,9,2]
        , [1,9,6,2,4,7,8,3,5]
        , [4,2,8,9,3,5,1,6,7]
        , [3,7,5,1,9,4,6,2,8]
        , [6,1,9,5,2,8,3,7,4]
        , [2,8,4,7,6,3,9,5,1]]).

:- end_tests(individueel).
