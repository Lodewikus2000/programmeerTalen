% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% Februari 2020
% Solution2, calculates the shortest Path is a graph.

:- consult('graph.pl').
:- consult('solution1.pl'). % use path from solution1.

% Calculates cost of a given Path by adding the costs of the nodes.
cost([], 0).
cost([H | T], Cost) :-
    H = edge(_, _, Cost1),
    cost(T, Cost2),
    Cost is Cost1 + Cost2.

% The shortest path is the path within all possible paths with
% the lowest cost.
shortestPath(X, Y, SPath) :-
    findall( (Cost, Path), (path(X,Y,Path), cost(Path, Cost)), Paths),
    sort(Paths, SortedPaths),
    SortedPaths = [(_, SPath)|_].
