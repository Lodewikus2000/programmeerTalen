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

-export([add_wall/4, choose_random_wall/1, build_random_wall/1,
         get_all_walls/2, get_cell_walls/2,
         get_grid/0, get_open_spots/1, get_wall/3, has_wall/4, new_grid/2,
         print_grid/1, show_hlines/2, show_vlines/2  ]).

 % Starts with an empty board.
start_link() ->
    start_link({6, 6, []}).

% roster is 6 x 6

 % Starts with a preconfigured board.
start_link(Rooms) ->
    gen_server:start_link({local, rrr}, rooms, Rooms, []).


init(Rooms) -> {ok, Rooms}.


add_wall(X, Y, Dir, Grid) ->
    {N, M, List} = Grid,
    Present = has_wall(X, Y, Dir, Grid),
    Wall = get_wall(X, Y, Dir),
    Within = (X >= 0) and (X < N) and (Y >= 0) and (Y < M),
    case {Present, Wall, Within} of
      {true, _, _} ->
        already_present;
      {false, {{X1, Y1}, {X2, Y2}}, true} ->
          {N, M, List ++ [{{X1, Y1}, {X2, Y2}}] };
      {_, _, false} ->
          not_placed_error
      end.


build_random_wall(Grid) ->
    {M, N, List} = Grid,
    Wall = choose_random_wall(Grid),
    if
      Wall =:= full ->
        full;
      true ->
        {M, N, List ++ [Wall]}
    end.


choose_random_wall(Grid) ->
  Open_spots = get_open_spots(Grid),
  Length = length(Open_spots),
  if
    Length =:= 0 ->
      [];
    true ->
      Random = rand:uniform(Length),
      lists:nth(Random, Open_spots)
    end.




get_wall(X, Y, Dir) ->
    case Dir of
      north ->  {{X , Y - 1}, {X, Y}};
      west  ->  {{X - 1, Y}, {X, Y}};
      south ->  {{X, Y}, {X, Y + 1}};
      east  ->  {{X, Y}, {X + 1, Y}};
      _     -> no_dir
    end.


has_wall(X, Y, Dir, {_, _, List}) ->
    Get = get_wall(X,Y,Dir),
    case Get of
      no_dir ->
        false;
      T ->
        member(T, List)
      end.








new_grid(Width,Height) ->
    {Width, Height, []}.


get_grid() -> gen_server:call(rrr, read).

get_cell_walls(X,Y) ->
    [get_wall(X, Y, Dir) || Dir <- [north, east, south, west]].

get_all_walls(W, H) ->
  W_range = lists:seq(0, W-1),
  H_range = lists:seq(0, H-1),
  Listlist = [get_cell_walls(X, Y) || X <- W_range, Y <- H_range],
  List = lists:merge(Listlist),
  remove_dup(List).

remove_dup([]) -> [];
remove_dup([H|T]) ->
    Member = lists:member(H, T),
    case Member of
      true ->
        remove_dup(T);
      false ->
        [H] ++ remove_dup(T)
      end.

get_open_spots(Grid) ->
    {M, N, List} = Grid,
    All_walls = get_all_walls(M, N),
    lists:subtract(All_walls, List).

v_line(T, List, M) ->
  {{_, _},{X, _}} = T,
  Member = member(T, List),
  End = X =:= M,
  case {Member, End} of
    {false, false} ->
      "   ";
    {false, true} ->
      " ~n";
    {true, false} ->
      "|  ";
    {true, true} ->
      "|~n"
    end.


show_vlines(Row, {M, _, List}) ->
    Range = lists:seq(0, M),
    String_list = [ v_line({{X - 1, Row},{X, Row}}, List, M) || X <- Range ],
    join_strings(String_list).


h_line(T, List) ->
    Member = member(T, List),
    case Member of
      false ->
        "  ";
      true ->
        "--"
      end.


% Prints the row ABOVE the given row
show_hlines(Row, {M, _, List}) ->
    Range = lists:seq(0, M-1),
    String_list = ["+"] ++ [ h_line({{X, Row - 1},{X, Row}}, List) ++ "+" || X <- Range ] ++ ["~n"],
    join_strings(String_list).


print_grid(Grid) ->
    {_, N, _} = Grid,
    Range = lists:seq(0, N),
    String_list = [show_hlines(X, Grid) ++ show_vlines(X, Grid) || X <- Range],
    Intro = "~nGame board:~n~n",
    String = join_strings([Intro] ++ String_list),
    io:format(String).


join_strings([]) -> [];
join_strings([H|T]) ->
    H ++ join_strings(T).

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


handle_cast({write, T} , {N, M, List }) ->
    { noreply, {N, M, List ++ [T] }};

handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
