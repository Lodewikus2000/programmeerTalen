% Leo Schreuders
% Student ID 5742978
% programmeertalen

append([],X,X).
append([Head1 | Tail1], L2, [Head1 | Tail3]) :- append(Tail1, L2, Tail3).

palindroom(List) :- reverse(List, List).

sudoku9(Puzzle, Solution) :-

  Puzzle = Solution,

  Puzzle = [[X11, X12, X13, X14, X15, X16, X17, X18, X19],
            [X21, X22, X23, X24, X25, X26, X27, X28, X29],
            [X31, X32, X33, X34, X35, X36, X37, X38, X39],
            [X41, X42, X43, X44, X45, X46, X47, X48, X49],
            [X51, X52, X53, X54, X55, X56, X57, X58, X59],
            [X61, X62, X63, X64, X65, X66, X67, X68, X69],
            [X71, X72, X73, X74, X75, X76, X77, X78, X79],
            [X81, X82, X83, X84, X85, X86, X87, X88, X89],
            [X91, X92, X93, X94, X95, X96, X97, X98, X99]],

  Row1 = [X11, X12, X13, X14, X15, X16, X17, X18, X19],
  Row2 = [X21, X22, X23, X24, X25, X26, X27, X28, X29],
  Row3 = [X31, X32, X33, X34, X35, X36, X37, X38, X39],
  Row4 = [X41, X42, X43, X44, X45, X46, X47, X48, X49],
  Row5 = [X51, X52, X53, X54, X55, X56, X57, X58, X59],
  Row6 = [X61, X62, X63, X64, X65, X66, X67, X68, X69],
  Row7 = [X71, X72, X73, X74, X75, X76, X77, X78, X79],
  Row8 = [X81, X82, X83, X84, X85, X86, X87, X88, X89],
  Row9 = [X91, X92, X93, X94, X95, X96, X97, X98, X99],

  Col1 = [X11, X21, X31, X41, X51, X61, X71, X81, X91],
  Col2 = [X12, X22, X32, X42, X52, X62, X72, X82, X92],
  Col3 = [X13, X23, X33, X43, X53, X63, X73, X83, X93],
  Col4 = [X14, X24, X34, X44, X54, X64, X74, X84, X94],
  Col5 = [X15, X25, X35, X45, X55, X65, X75, X85, X95],
  Col6 = [X16, X26, X36, X46, X56, X66, X76, X86, X96],
  Col7 = [X17, X27, X37, X47, X57, X67, X77, X87, X97],
  Col8 = [X18, X28, X38, X48, X58, X68, X78, X88, X98],
  Col9 = [X19, X29, X39, X49, X59, X69, X79, X89, X99],

  Block1 = [X11, X12, X13, X21, X22, X23, X31, X32, X33],
  Block2 = [X41, X42, X43, X51, X52, X53, X61, X62, X63],
  Block3 = [X71, X72, X73, X81, X82, X83, X91, X92, X93],
  Block4 = [X14, X15, X16, X24, X25, X26, X34, X35, X36],
  Block5 = [X44, X45, X46, X54, X55, X56, X64, X65, X66],
  Block6 = [X74, X75, X76, X84, X85, X86, X94, X95, X96],
  Block7 = [X17, X18, X19, X27, X28, X29, X37, X38, X39],
  Block8 = [X47, X48, X49, X57, X58, X59, X67, X68, X69],
  Block9 = [X77, X78, X79, X87, X88, X89, X97, X98, X99],

  valid(Row1),
  valid(Row2),
  valid(Row3),
  valid(Row4),
  valid(Row5),
  valid(Row6),
  valid(Row7),
  valid(Row8),
  valid(Row9),

  valid(Col1),
  valid(Col2),
  valid(Col3),
  valid(Col4),
  valid(Col5),
  valid(Col6),
  valid(Col7),
  valid(Col8),
  valid(Col9),


  valid(Block1),
  valid(Block2),
  valid(Block3),
  valid(Block4),
  valid(Block5),
  valid(Block6),
  valid(Block7),
  valid(Block8),
  valid(Block9),

  printSudoku(Puzzle).

printSudoku([]).
printSudoku([Row | Rows]) :-
  print(Row),
  nl(),
  printSudoku(Rows).

valid(X) :-
  all_unique(X).
  % length(X, 9),
  % member(1, X),
  % member(2, X),
  % member(3, X),
  % member(4, X),
  % member(5, X),
  % member(6, X),
  % member(7, X),
  % member(8, X),
  % member(9, X).


all_unique([]).
all_unique([H|T]) :- all_unique(T),
                     member(H, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
                     not(member(H, T)).
