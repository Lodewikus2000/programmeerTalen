:- begin_tests(solution5).

:- op(100, xfx, at).
:- op(75, xfx, ><). % Arrival >< Departur
:- op(50, xfx, :).


test(costArrDep) :-
    solution5:cost([arrives(_ at 10:00), departs(_ at 10:05)], 5).


test(costArrWaitDep) :-
    solution5:cost([arrives(_ at 10:00), wait(_, 10), departs(_ at 10:10)], 10).


test(shortestPath) :-
    solution5:shortestPath("Amsterdam Centraal"at 10:05,
                           "Utrecht Centraal"at _, Schedule),
    Schedule ==
    [ departs("Amsterdam Centraal"at 10:5)
    , travel("Amsterdam Centraal","Amsterdam Amstel", 8)
    , arrives("Amsterdam Amstel"at 10:13)
    , wait("Amsterdam Amstel", 9), departs("Amsterdam Amstel"at 10:22)
    , travel("Amsterdam Amstel", "Utrecht Centraal", 18)
    , arrives("Utrecht Centraal"at 10:40)
    ].

:- end_tests(solution5).
