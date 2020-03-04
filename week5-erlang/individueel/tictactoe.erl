-module(tictactoe).
-behaviour(gen_server).


-export([start_link/0, start_link/1, print_board/0, print_board/1, show_board/1,
     restart/0, restart/1, move/2, is_finished/0, get_board/0]).
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).


% Starts with an empty board.
start_link() ->
    start_link([]).


% Starts with a preconfigured board.
start_link(Board) ->
    gen_server:start_link({local, ttt}, tictactoe, Board, []).


init(Board) -> {ok, Board}.


restart() ->
    restart([]).


restart(Board) ->
    gen_server:stop(ttt),
    start_link(Board).


move(X,Y) ->

    Won = anyone_won(),
    case Won of

      nobody ->

          Board = get_board(),

          % Check if these coordinates are already occupied.
          Member = is_member({X, Y}, Board),
          % Check if the coordiantes are on the board.
          Valid = (0 =< X) and (X =< 2) and ( 0 =< Y) and (Y =< 2),

          if
            Member ->
              not_open;
            not Valid ->
              not_valid;
            true ->
              gen_server:cast(ttt, {write, X, Y, 1}),
              Finished = is_finished(),
              if
                Finished ->
                  % If the game is finished, return who won (if anyone).
                  anyone_won();
                true ->
                  move_PC(),
                  ok
                end
            end;
        _ ->
          Won
        end.


move_PC() ->
    Moves = [{X,Y} || X <- [0,1,2], Y <- [0,1,2]],
    Board = get_board(),
    move_PC1(Moves, Board).


move_PC1([{X,Y} | T], Board) ->
    Member = is_member({X, Y}, Board),
    if
      Member ->
        move_PC1(T, Board);
      true ->
        gen_server:cast(ttt, {write, X, Y, 2})
      end.


is_finished() ->
    Board = get_board(),
    if
      % The board is finished if it's full.
      length(Board) =:= 9 ->
        true;
      % If the board is not full, it is finished if someone won.
      true ->
        Won = anyone_won(),
        case Won of
          nobody ->
            false;
          _ ->
            true
          end
      end.


anyone_won() ->
    Board = get_board(),
    Row0 = [replace(X,0, Board) || X <- [0,1,2]],
    Row1 = [replace(X,1, Board) || X <- [0,1,2]],
    Row2 = [replace(X,2, Board) || X <- [0,1,2]],
    Col0 = [replace(0,Y, Board) || Y <- [0,1,2]],
    Col1 = [replace(1,Y, Board) || Y <- [0,1,2]],
    Col2 = [replace(2,Y, Board) || Y <- [0,1,2]],
    Diag1 = [replace(X,X, Board) || X <- [0,1,2]],
    Diag2 = [replace(0,2, Board), replace(1,1, Board), replace(2,0, Board)],
    All = [Row0, Row1, Row2, Col0, Col1, Col2, Diag1, Diag2],
    won(All).


won([H]) ->
    case H of
      ["O", "O", "O"] ->
        {won, 1};
      ["X", "X", "X"] ->
        {won, 2};
      [_, _, _] ->
        nobody
      end;
won([H|T]) ->
    case H of
      ["O", "O", "O"] ->
        {won, 1};
      ["X", "X", "X"] ->
        {won, 2};
      [_, _, _] ->
        won(T)
      end.


get_board() -> gen_server:call(ttt, read).


show_board(Board) ->
    % The assignment switches X and Y for some reason.
    String = [replace(Y,X, Board) ++ pretty(Y,X) || X <- [0,1,2], Y <- [0,1,2]],
    join_strings(String).


% This function joins a list of strings.
join_strings([]) -> [];
join_strings([H|T]) ->
    H ++ join_strings(T).


% Check if a coordinate pair is used in the current board.
is_member(_, []) -> false;
is_member({X, Y}, [H|T]) ->
  if
    ({X, Y, 1} =:= H ) or ({X, Y, 2} =:= H )  ->
      true;
    true ->
      is_member({X, Y}, T)
  end.


% T translate a coordinate in the board to either a "O", "X", or " ".
replace(_,_, []) -> " ";
replace(X,Y, [H|T]) ->
    case H of
      {X, Y, 1} ->
        "O";
      {X, Y, 2} ->
        "X";
      {_,_,_} ->
        replace(X, Y, T)
    end.


pretty(X,Y) ->
  case {X,Y} of
    {0, _} ->
      "|";
    {1, _} ->
      "|";
    {2, 0} ->
      "~n------~n";
    {2, 1} ->
      "~n------~n";
    {2, 2} ->
      "~n"
    end.


%% TODO: Add the required calls.
handle_call(read, _From, State) ->
  { reply, State, State };
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({write, X,Y,P} , State) ->
  { noreply, State ++ [{X,Y,P}] };
handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

% This is just a helper for in the REPL.
print_board() ->
    print_board(get_board()).

% This allows you to test printing without the server working.
print_board(Board) ->
    io:fwrite(show_board(Board)).
