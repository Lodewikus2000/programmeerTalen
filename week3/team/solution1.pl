:- consult('graph.pl').

path(X, Y, Path) :-
    path1(X, Y, [], Path).


path1(From, To, Visited, Path) :-
    edge(From, X, Cost),
    not(member(edge(_, X, _), Visited)),
    NewVisited = [edge(From, X, Cost) | Visited],
    (
    (X = To, reverse(NewVisited, Path) ) ;
    ( X \= To, path1(X, To, NewVisited, Path))
    ).
