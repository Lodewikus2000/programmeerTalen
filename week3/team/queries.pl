% Pim van Helvoirt, STUDENT ID
% Leo Schreuders, 5742978
% Programmeertalen
% februari 2020


% Welke paden zijn er van 1 naar 3, van 3 naar 5 en van 5 naar 4?
:- [solution1],
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


:- [solution2],
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


  % Hoeveel minuten kost het om van Amsterdam Amstel naar Sittard te reizen?
:-
  [solution3],
  write("Aantal minuten tussen Amsterdam Amstel en Sittard:"),
  nl,
  shortestPath("Amsterdam Amstel"at _, "Sittard"at _, Path),
  cost(Path, Cost),
  write(Cost),
  nl,
  nl.



% Tussen ’s-Hertogenbosch en Eindhoven zijn er werkzaamheden. Er worden bussen ingezet tussen deze twee stations.
% De bus vertrekt vijf minuten na de aankomsttijd, en de rit duurt zo’n dertig minuten. Hoeveel minuten kost het
% om van Amsterdam Centraal naar Maastricht te gaan?

:- [solution3],
  path("Amsterdam Centraal"at _, "'s-Hertogenbosch"at Time1, Path1),
  path("Eindhoven" at Time2, "Maastricht" at _, Path2),
  diffTime(Time2, Time1, Diff),
  Diff > 35,
  nl,
  append(Path1, Path2, Path3),
  cost(Path3, Cost),
  write("De duur van de rit van Amsterdam Centraal naar Maastricht, inclusief een busreis van 30 minuten die 5 minuten na aankomst vertrekt uit Den Bosch:"),
  nl,
  write(Cost),
  nl.




% Wat is het reisplan van Amsterdam Science Park naar ’s-Hertogenbosch, hoeveel minuten kost het en hoevaak
% moeten we overstappen?
:- op(100, xfx, at).
:- op(50, xfx, :).

:-
  consult('route.pl'),
  consult('solution4.pl'),
  shortestPath("Amsterdam Science Park"at X,"'s-Hertogenbosch" at Y, Path2),
  write(Path2).
