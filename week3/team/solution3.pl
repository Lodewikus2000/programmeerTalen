:- consult('route.pl').

edge(From, To, Cost) :-
  route(Route),
  findEdge(From, To, Cost, Route).


findEdge(From, To, Cost, Route) :-
  Route = [F, S | T],
  (
  (
  F = From at Time1,
  S = To at Time2,
  diffTime(Time1, Time2, Cost)
  );
  findEdge(From, To, Cost, [S|T])
  ).








diffTime(H1:M1, H2:M2, Minutes) :- Minutes is (H2*60 + M2) - (H1* 60 + M1).
