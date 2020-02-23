:- consult('route.pl').

path(X, Y, Path) :-
  path1(X, Y, [], Path).

% Calcute path based on Solution3, now taking before time and wait time into
% account.
path1(From at Time1, To, Visited, Path) :-
    (
        (
        edge(From at Time1, X, Cost),
        not(member(travel(_, X, _), Visited)),
        NewVisited = [travel(From at Time1, X, Cost) | Visited]
        ) ;
        (
        edge(From at Time2, X, Cost),
        before(Time1, Time2),
        diffTime(Time2, Time1, WaitTime),

        NewVisited = [travel(From at Time2, X, Cost),
                      wait(From, WaitTime) | Visited]
        )
    ),
    (
        (
        X = To,
        reverse(NewVisited, Path)
        ) ;
        (
        X \= To,
        path1(X, To, NewVisited, Path)
        )
    ).

% Cost, but now add a special wait predicate with a cost too.
cost([], 0).
cost([H | T], Cost) :-
    (H = travel(_, _, Cost1) ; H = wait(_ , Cost1)),
    cost(T, Cost2),
    Cost is Cost1 + Cost2.

shortestPath(X, Y, SPath) :-
    findall( (Cost, Path), (path(X, Y, Path), cost(Path, Cost)), Paths),
    sort(Paths, SortedPaths),
    SortedPaths = [(_, SPath)|_].

% An edge exists if there is a route in which it can be found.
edge(From, To, Cost) :-
    route(Route),
    findedge(From, To, Cost, Route).

% Find a edge with a diff Time.
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


diffTime(H1:M1, H0:M0, Minutes) :-
    \+ var(H1),
    \+ var(H0),
    Minutes is (H1*60 + M1) - (H0* 60 + M0).

diffTime(H1:_, H2:_, Minutes) :-
    (
    var(H1);
    var(H2)
    ),
    Minutes is 0.

%
before(Time1, Time2) :-
    diffTime(Time2, Time1, Difference),
    Difference > 0.
