% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% Maart 2020
% Rooms
% Implement a game of 'kamertje verhuren'.

-module(rooms).
-behaviour(gen_server).


-export([get_wall/3, new_grid/2]).

 % Starts with an empty board.
start_link() ->
    start_link([]).

% roster is 6 x 6

 % Starts with a preconfigured board.
start_link(Board) ->
    gen_server:start_link({local, ttt}, tictactoe, Board, []).


init(Board) -> {ok, Board}.


% TO DO:
%  Dir = {North, East, South, West}
get_wall(X, Y, Dir) -> ok.

new_grid(Width, Height) -> ok.



% NOTE: TO DO: change to new handlers.

restart() ->
    restart([]).

restart(Board) ->
    gen_server:stop(ttt),
    start_link(Board).

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
