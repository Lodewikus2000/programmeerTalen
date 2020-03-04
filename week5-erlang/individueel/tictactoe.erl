% OKE LEO DIT MOET JE NOG DOEN:
%
% call stuurt altijd antwoord terug, cast niet
%
%
%
%
%
%
%
%


-module(tictactoe).
-import(lists,[last/1]).
-behaviour(gen_server).

-export([start_link/0, start_link/1, print_board/0, print_board/1, show_board/1,
     restart/0, restart/1, move/2, is_finished/0, get_board/0]).
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

% Starts with an empty board.
start_link() ->
    start_link([]).

% Starts with a preconfigured board.
% tictactoe omdat de module zo heet
start_link(Board) ->
    gen_server:start_link({local, ttt}, tictactoe, Board, []).

init(Board) -> {ok, Board}.

%%% TODO: implement these functions.
restart() ->
    ok.

restart(Board) ->
    ok.

move(X,Y) ->
  Board = get_board(),

  Member = is_member({X, Y}, Board),
  Valid = (0 =< X) and (X =< 2) and ( 0 =< Y) and (Y =< 2),
  if
    Board =:= [] ->
      Player = 1;
    true ->
      {_, _, Previous_player} = last(Board),
      Player = (Previous_player rem 2) + 1
    end,

  if
    Member ->
      not_open;
    not Valid ->
      not_valid;
    true ->
      gen_server:cast(ttt, {write, X, Y, Player})
    end.

is_finished() ->
    ok.

get_board() -> gen_server:call(ttt, read).

show_board(Board) ->
    String = [filled_with(Y,X, Board) ++ fill(Y,X) || X <- [0,1,2], Y <- [0,1,2]],
    flatten(String).

% Van stackoverflow https://stackoverflow.com/questions/9344785/flatten-a-list-of-nested-lists-in-erlang
flatten(X)               -> flatten(X,[]).
flatten([],Acc)          -> Acc;
flatten([[]|T],Acc)      -> flatten(T, Acc);
flatten([[_|_]=H|T],Acc) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc)       -> flatten(T,Acc++[H]) .

is_member(_, []) -> false;
is_member({X, Y}, [H|T]) ->
  if
    ({X, Y, 1} =:= H ) or ({X, Y, 2} =:= H )  ->
      true;
    true ->
      is_member({X, Y}, T)
  end.

filled_with(_,_, []) -> " ";
filled_with(X,Y, [H|T]) ->
    case H of
      {X, Y, 1} ->
        "O";
      {X, Y, 2} ->
        "X";
      {_,_,_} ->
        filled_with(X, Y, T)
    end.

fill(X,Y) ->
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
% omdat we de state niet aanpassen, kunnen we hergebruiken
handle_call(read, _From, State) ->
  { reply, State, State }.

% handle_call(terminate, _From, State) ->
%     {stop, normal, ok, State}.

handle_cast({write, X,Y,P} , State) ->
  { noreply, State ++ [{X,Y,P}] }.
% handle_cast(restart, _State) ->
%     {noreply, []}.

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
