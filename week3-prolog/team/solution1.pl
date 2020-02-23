% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% Februari 2020
% Solution1, calculates paths in a Graph.

:- consult('graph.pl').

path(X, Y, Path) :-
    path1(X, Y, [], Path).

% Find a path from a node to a node.
path1(From, To, Visited, Path) :-
    edge(From, X, Cost),
    % New nodes should not be Visited.
    not(member(edge(_, X, _), Visited)),
    NewVisited = [edge(From, X, Cost) | Visited],

    % Flip the path before returning the final result.
    (
    (X = To, reverse(NewVisited, Path) ) ;
    ( X \= To, path1(X, To, NewVisited, Path))
    ).
