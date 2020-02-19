:- consult('route.pl').

edge(From, To, Cost) :-
  route(Route),
  findEdge(From, To, Cost, Route).


findEdge(From at Time1, To at Time2, Cost, Route) :-
  Route = [F, S | T],
  (
  (
  F = From at Time1,
  S = To at Time2,
  diffTime(Time2, Time1, Cost)
  );
  findEdge(From at Time1, To at Time2, Cost, [S|T])
  ).








diffTime(H2:M2, H1:M1, Minutes) :- Minutes is (H2*60 + M2) - (H1* 60 + M1).
