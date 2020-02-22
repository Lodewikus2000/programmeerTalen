% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% februari 2020

:- use_module(library(occurs)).

:- set_prolog_flag(answer_write_options,
                   [ quoted(true), portray(true), spacing(next_argument)]).

% Welke paden zijn er van 1 naar 3, van 3 naar 5 en van 5 naar 4?
test1 :- consult('solution1.pl'),
    [solution1],
    write("-------------------"),
    nl,
    write("Opdracht 1"),
    nl,
    write("-------------------"),
    nl,
    nl,
    findall(Path1, path(1,3,Path1), List1),
    write("paden van 1 naar 3:"),
    nl,
    write(List1),
    nl,
    nl,

    findall(Path2, path(3,5,Path2), List2),
    write("paden van 3 naar 5:"),
    nl,
    write(List2),
    nl,
    nl,

    findall(Path3, path(5,4,Path3), List3),
    write("paden van 5 naar 4:"),
    nl,
    write(List3),
    nl,
    nl,

    nl.

test2 :- consult('solution2.pl'),
    [solution2],
    % Wat zijn de kosten van ieder pad van 5 naar 4 om deze te bewandelen?
    write("-------------------"),
    nl,
    write("Opdracht 2"),
    nl,
    write("-------------------"),
    nl,
    nl,
    findall(Path, path(5, 4, Path), List),
    maplist(cost, List, List2),
    write("de kosten van alle paden van 5 naar 4:"),
    nl,
    write(List2),
    nl,
    nl,

    % Wat zijn de kortste paden van 1 naar 3, van 3 naar 5 en van 5 naar 4?
    shortestPath(1,3, Path2),
    write("kortste pad van 1 naar 3:"),
    nl,
    write(Path2),
    nl,
    nl,

    shortestPath(3,5, Path3),
    write("kortste pad van 3 naar 5:"),
    nl,
    write(Path3),
    nl,
    nl,

    shortestPath(5,4, Path4),
    write("kortste pad van 5 naar 4:"),
    nl,
    write(Path4),
    nl,
    nl.

test3 :- consult('solution3.pl'),
% Hoeveel minuten kost het om van Amsterdam Amstel naar Sittard te reizen?
    [solution3],
    write("Aantal minuten tussen Amsterdam Amstel en Sittard:"),
    nl,
    shortestPath("Amsterdam Amstel" at _, "Sittard" at _, Path),
    cost(Path, Cost),
    write(Cost),
    nl,
    nl,

% Tussen ’s-Hertogenbosch en Eindhoven zijn er werkzaamheden.
% Er worden bussen ingezet tussen deze twee stations.
% De bus vertrekt vijf minuten na de aankomsttijd,
% en de rit duurt zo’n dertig minuten. Hoeveel minuten kost het
% om van Amsterdam Centraal naar Maastricht te gaan?

    write("De duur van de rit van Amsterdam Centraal naar Maastricht, \c
          inclusief een busreis van 30 minuten die 5 minuten \c
          na aankomst vertrekt uit Den Bosch:"),
    path("Amsterdam Centraal" at _, "'s-Hertogenbosch" at Time1, P1),
    path("Eindhoven" at Time2, "Maastricht" at _,  P2),
    diffTime(Time2, Time1, Diff1),
    Diff1 > 35,
    cost(P1, Cost1),
    cost(P2, Cost2),
    TotalCost is Cost1 + Cost2 + 35,
    nl,
    write(TotalCost),
    nl,
    nl.

% Wat is het reisplan van Amsterdam Science Park naar ’s-Hertogenbosch,
% hoeveel minuten kost het en hoevaak moeten we overstappen?
:- op(100, xfx, at).
:- op(50, xfx, :).

test4 :- consult('solution4.pl'),
    [solution4],
    shortestPath("Amsterdam Science Park" at _,
                 "'s-Hertogenbosch" at _, S),
    write(S).

test5 :- consult('solution5.pl'),
    consult('route5.pl'),
    Begin = 11:16,
    shortestPath("Schiphol Airport" at Begin, "Nijmegen" at End, Schedule),
    write(Schedule),
    occurrences_of_term(travel(_,_,_), Schedule, Overstappen),
    nl,

    write("Overstappen: "), write(Overstappen), nl,
    diffTime(End, Begin, Diff),
    write("Reisduur: "), write(Diff),
    nl.
