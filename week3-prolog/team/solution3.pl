% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% Februari 2020
% Solution3, calculates the shortest Path from a route.

:- consult('route.pl').

% Redefine the path rule from solution 1, to unpack travel predicate.
path(X, Y, Path) :-
    path1(X, Y, [], Path).


path1(From, To, Visited, Path) :-
    edge(From, X, Cost),
    not(member(travel(_, X, _), Visited)),

    NewVisited = [travel(From, X, Cost) | Visited],
    (
        (X = To, reverse(NewVisited, Path) ) ;
        ( X \= To, path1(X, To, NewVisited, Path))
    ).

% Based on same principle as cost in Solution 2, calculates cost
% of a path.
cost([], 0).
cost([H | T], Cost) :-
    H = travel(_, _, Cost1),
    cost(T, Cost2),
    Cost is Cost1 + Cost2.

% Finds the shortest Path between to places.
shortestPath(X, Y, SPath) :-
        findall( (Cost, Path), (path(X,Y,Path), cost(Path, Cost)), Paths),
    sort(Paths, SortedPaths),
    SortedPaths = [(_, SPath)|_],

    X = From at Dep,
    Y = To at Arr,

    SPath = [ travel(From at Dep, _ at _ , _)|_],
    reverse(SPath, ReverseSPath),
    ReverseSPath = [ travel(_ at _, To at Arr, _)|_].



% An edge exists if there is a route in which it can be found.
edge(From, To, Cost) :-
    route(Route),
    findedge(From, To, Cost, Route).


findedge(From at Time1, To at Time2, Cost, Route) :-
    Route = [F, S | T],
    (
      (
      F = From at Time1,
      S = To at Time2,
      diffTime(Time2, Time1, Cost)
      );
      findedge(From at Time1, To at Time2, Cost, [S|T])
    ).

% Calculates the difference in time in minutes between to Time objects.
diffTime(H1:M1, H0:M0, Minutes) :-
    % ensure that both are not free variables.
    \+ var(H1),
    \+ var(H0),

  Minutes is (H1*60 + M1) - (H0* 60 + M0).

diffTime(H1:_, H2:_, Minutes) :-
      (
      % if either is a free variable difference in time is not a thing,
      % and should be zero.
      var(H1);
      var(H2)
      ),
      Minutes is 0.
