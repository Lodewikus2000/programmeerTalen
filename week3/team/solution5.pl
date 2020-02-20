:- consult('route5.pl').

path(X, Y, Path) :-
  path1(X, Y, [], Path).


path1(From at Dep, To at Arr, Visited, Path) :-
  (
    (
      edge(From at Dep, X at Arr1, Cost),
      % write((From at Time1, X, Cost)),
      % nl,
      not(member(travel(_, X at _, _), Visited)),
      NewVisited = [arrives(X at Arr1), travel(From, X, Cost), departs(From at Dep) | Visited]
    )
    ;
    (
      edge(From at Dep2, X at Arr1, Cost),
      % write((From at Time2, X, Cost)),
      % nl,
      before(Dep, Dep2),
      diffTime(Dep2, Dep, WaitTime),

      NewVisited = [arrives(X at Arr1), travel(From, X, Cost), departs(From at Dep2), wait(From, WaitTime) | Visited]
    )
  ),
  (
    (
      X at Arr1 = To at Arr,
      % write(X at Arr1),
      % write(" =? "),
      % write(To at Arr),
      % nl,
      reverse(NewVisited, Path)
      % write(Path),
      % nl,
      % write("we arrived"),
      % nl,
      % nl

    ) ;
    (
      X at Arr1 \= To at Arr,
      % reverse(NewVisited, Reversed),
      % write(Reversed),
      % nl,
      % write("not there yet"),
      % nl,
      % nl,
      path1(X at Arr1, To at Arr, NewVisited, Path)
    )
  ).




% cost([], 0).
% cost([H | T], Cost) :-
%   (
%   H = travel(_, _, Cost1);
%   H = wait(_ , Cost1);
%   H=
%   ),
%   cost(T, Cost2),
%   Cost is Cost1 + Cost2.

cost(Path, Cost) :-
  Path = [H1 | _],
  last(Path, H2),
  (H1 = departs(_ at T1) ; H1 = arrives(_ at T1) ),
  (H2 = departs(_ at T2) ; H2 = arrives(_ at T2) ),
  diffTime(T2, T1, Cost).


shortestPath(X, Y, SPath) :-
  findall( (Cost, Path), (path(X, Y, Path), cost(Path, Cost)), Paths),
  sort(Paths, SortedPaths),
  SortedPaths = [(_, SPath)|_].



% An edge exists if there is a route in which it can be found.
edge(From at Dep, To at Arr, Cost) :-
  route(Route),
  findedge(From at Dep, To at Arr, Cost, Route).


findedge(From at Dep, To at Arr, Cost, Route) :-
  Route = [F, S | T],
  (
  (
  F = From at _ >< Dep,
  S = To at Arr >< _,
  diffTime(Arr, Dep, Cost)
  );
  findedge(From at Dep, To at Arr, Cost, [S|T])
  ).


diffTime(H1:M1, H0:M0, Minutes) :- Minutes is (H1*60 + M1) - (H0* 60 + M0).

before(Time1, Time2) :-
  diffTime(Time2, Time1, Difference),
  Difference > 0.
