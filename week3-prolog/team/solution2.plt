:- begin_tests(solution2).

test(cost) :-
    assertion(solution2:cost([edge(3, 2, 2), edge(2, 5, 5),
                              edge(5, 1, 3)], 10)),
    assertion(solution2:cost([edge(3, 2, 2), edge(2, 5, 5),
                              edge(5, 1, 3), edge(1, 2, 5)], 15)),
    assertion(solution2:cost([edge(1, 2, 5), edge(2, 3, 4), edge(3, 1, 9),
                              edge(1, 2, 5)], 23)).


test(shortestPath) :-
    assertion(solution2:shortestPath(3, 1, [edge(3, 2, 2), edge(2, 1, 3)])),
    assertion(solution2:shortestPath(1, 5, [edge(1, 2, 5), edge(2, 5, 5)])),
    assertion(solution2:shortestPath(1, 4, [edge(1, 2, 5), edge(2, 4, 3)])).


% If this test fails, you're likely stating that
% the shortestPath predicate evaluates to _Path = false.
% Instead it should evaluate to false.
test(shortestPath42, fail) :-
    solution2:shortestPath(4, 2, _Path).

:- end_tests(solution2).