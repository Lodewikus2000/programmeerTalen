% Pim van Helvoirt, 10546413
% Leo Schreuders, 5742978
% Programmeertalen
% Maart 2020
% Rooms
% Implement a game of 'kamertje verhuren'.

-module(rooms).
-behaviour(gen_server).

-import(lists, [member/2]).

-export([init/1, start_link/0, start_link/1, restart/0, restart/3,
         handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-export([get_wall/3, get_grid/0, has_wall/4, add_wall/4]).

 % Starts with an empty board.
start_link() ->
    start_link({6, 6, []}).

% roster is 6 x 6

 % Starts with a preconfigured board.
start_link(Room) ->
    gen_server:start_link({local, rrr}, rooms, Room, []).


init(Room) -> {ok, Room}.



% TO DO:
%  Dir = {North, East, South, West}
get_wall(X, Y, Dir) ->
    {N, M, _} = get_grid(),
    Within = ((X >= 0) and (X < N) and (Y >=0) and (Y < M)),
    case {Within, Dir} of
      {true, north} -> {{X - 1, Y}, {X, Y}};
      {true, west} ->  {{X, Y - 1}, {X, Y}};
      {true, south} -> {{X, Y}, {X + 1, Y}};
      {true, east} ->  {{X, Y}, {X, Y + 1}};
      {true, _} -> no_dir;
      {false, _} -> not_within
    end.


has_wall(X, Y, Dir, {_, _, List}) ->
    Get = get_wall(X,Y,Dir),
    case Get of
      no_dir ->
        false;
      not_within ->
        false;
      T ->
        member(T, List)
      end.

add_wall(X, Y, Dir, Grid) ->
    Present = has_wall(X, Y, Dir, Grid),
    Wall = get_wall(X, Y, Dir),
    case {Present, Wall} of
      {true, _} ->
        already_present;
      {false, {{X1, Y1}, {X2, Y2}}} ->
          gen_server:cast(rrr, {write, {{X1, Y1}, {X2, Y2}}});
      {false, _} ->
        not_placed_error
      end.





get_grid() -> gen_server:call(rrr, read).




% NOTE: TO DO: change to new handlers.

restart() ->
    restart(6, 6, []).

restart(X, Y, Walls) ->
    gen_server:stop(rrr),
    start_link({X, Y, Walls}).

handle_call(read, _From, State) ->
    { reply, State, State };

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.


handle_cast({write, T} , {M, N, List }) ->
    { noreply, {M, N, List ++ [T] }};

handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
