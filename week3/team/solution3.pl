:- consult('route.pl').

edge(From at Time1, To at Time2, Cost) :-
  findEdge(From, To, Cost, route(_)),
  write(route),
  nl().


findEdge( From, To, Cost, [X, Y | T]) :-
  X = From at Time1,
  Y = To at Time2,
  Cost = diffTime(Time1, Time2);
  findedge( From, To, Cost, [Y |T]).





diffTime(H1:M1, H0:M0, Minutes) :- Minutes is (H1-H0) * 60 + (M1-M0).
