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



cost([], 0).
cost([H | T], Cost) :-
  H = edge(_, _, Cost1),
  cost(T, Cost2),
  Cost is Cost1 + Cost2.

shortestPath(X, Y, SPath) :-
  findall( (Cost, Path), (path(X,Y,Path), cost(Path, Cost)), Paths),
  sort(Paths, SortedPaths),
  SortedPaths = [(_, SPath)|_].
