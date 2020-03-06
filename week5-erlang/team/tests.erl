-module(tests).

%% Subset of the tests on Codegrade.
% they should mostly give an indication of what is expected
% and use small fields. Try bigger ones yourself!

-include_lib("eunit/include/eunit.hrl").

-export([run_tests/0]).

-import(rooms, []).
-import(lists, []).
-import(init, [stop/1]).

run_tests() ->
    eunit:test(tests),
    stop(0).

build_grid_test() ->
    ?assertEqual({6, 6, []}, rooms:new_grid(6, 6)),
    ?assertEqual({1, 1, []}, rooms:new_grid(1, 1)).

get_wall_test() ->
    ?assertEqual({{0,-1},{0,0}}, rooms:get_wall(0, 0, north)),
    ?assertEqual({{5,5},{5,6}}, rooms:get_wall(5, 5, south)),
    ?assertEqual({{1,1},{2,1}}, rooms:get_wall(1, 1, east)),
    ?assertEqual({{-1,0},{0,0}}, rooms:get_wall(0, 0, west)).

has_wall_test() ->
    ?assertEqual(false, rooms:has_wall(5, 3, west, {6, 6, []})),
    ?assertEqual(true, rooms:has_wall(0, 0, north, {6, 6, [{{0, -1}, {0, 0}}]})).

add_wall_test() ->
    Grid = rooms:new_grid(4, 6),
    ?assertEqual({4, 6, [{{1,0},{1,1}}]}, rooms:add_wall(1, 1, north, Grid)),
    Grid1 = rooms:new_grid(1, 1),
    ?assertEqual({1, 1, [{{0,0},{1,0}}]}, rooms:add_wall(0, 0, east, Grid1)).


% So, | first vertical wall({-1, 0}, {0, 0}), then two spaces,
% then sec vertical wall({{0, 0}, {1, 0}}).
% Newline is required.
show_vlines_test() ->
    ?assertEqual(
        "|  |~n",
        rooms:show_vlines(0, {1, 1, [{{-1, 0}, {0, 0}}, {{0, 0}, {1, 0}}]})
    ),
    ?assertEqual("    ~n", rooms:show_vlines(0, {1, 1, []})). % No walls in this line.

% So, '+' is a corner of a room and is always there. -- is a horizontal wall.
% First test does not have any walls. So: a corner point, two spaces and a corner point.
% Second test has a wall. Therefore: a corner point, a wall(--) and another corner point.
% Newlines are also required.
show_hlines_test() ->
    ?assertEqual("+  +~n", rooms:show_hlines(0, {1, 1, [{{0, 0}, {0, 1}}]})),
    ?assertEqual("+--+~n", rooms:show_hlines(1, {1, 1, [{{0, 0}, {0, 1}}]})).

% Combining hlines and vlines we would get the following grid:
% rooms:print_grid({1, 1, [{{-1, 0}, {0, 0}}, {{0, 0}, {1, 0}}, {{0, 0}, {0, 1}}]}).
% +  +
% |  |
% +--+

% Then, you should extrapolate this for an NxM grid where N and M > 0


get_cell_walls_test() ->
    ?assertEqual([{{1,2},{2,2}},{{2,1},{2,2}},{{2,2},{2,3}},{{2,2},{3,2}}],
        lists:sort(rooms:get_cell_walls(2, 2))),
    ?assertEqual([{{6,9},{7,9}},{{7,8},{7,9}},{{7,9},{7,10}},{{7,9},{8,9}}],
        lists:sort(rooms:get_cell_walls(7, 9))).

get_all_walls_test() ->
    ?assertEqual(
        [ {{-1,0},{0,0}}
        , {{-1,1},{0,1}}
        , {{0,-1},{0,0}}
        , {{0,0},{0,1}}
        , {{0,0},{1,0}}
        , {{0,1},{0,2}}
        , {{0,1},{1,1}}
        ],
        lists:sort(rooms:get_all_walls(1, 2))
    ).

get_open_spots_test() ->
    ?assertEqual(
        [{{-1,0},{0,0}},{{0,-1},{0,0}}],
        lists:sort(rooms:get_open_spots({1, 1, [{{0, 0}, {0, 1}}, {{0, 0}, {1, 0}}]}))
    ),
    ?assertEqual(
        [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}},{{1,-1},{1,0}}],
        lists:sort(rooms:get_open_spots((
            {2, 1, [{{1, 0}, {2, 0}}, {{0, 0}, {1, 0}}, {{1, 0}, {1, 1}}]}))
        )
    ).

choose_random_wall_test() ->
    ?assertEqual(
        [],
        rooms:choose_random_wall(
            {1, 1, [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}},{{0,0},{1,0}}]})
        ),
    ?assertEqual(
        {{0,0},{1,0}},
        rooms:choose_random_wall({1, 1, [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}}]})
    ).

build_random_wall_test() ->
    ?assertEqual(
        {1,1, [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}},{{0,0},{1,0}}]},
        rooms:build_random_wall({1, 1, [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}}]})
    ).

get_open_cell_walls_test() ->
    ?assertEqual([{{0,1},{1,1}},{{1,0},{1,1}},{{1,1},{1,2}},{{1,1},{2,1}}],
        lists:sort(rooms:get_open_cell_walls(1, 1, {5, 10, []}))
    ),
    ?assertEqual([],
        rooms:get_open_cell_walls(0, 0,
            {1, 1, [{{0,0},{1,0}},{{0,0},{0,1}},{{-1,0},{0,0}},{{0,-1},{0,0}}]})
    ),
    ?assertEqual([{{0,-1},{0,0}}],
        rooms:get_open_cell_walls(0, 0,
            {1, 1, [{{0,0},{1,0}},{{0,0},{0,1}},{{-1,0},{0,0}}]})
    ).

get_completable_walls_test() ->
    ?assertEqual([], rooms:get_completable_walls({1, 1, []})),
    ?assertEqual([{{0,-1},{0,0}}],
        rooms:get_completable_walls({1, 1,
            [{{0,0},{1,0}},{{0,0},{0,1}},{{-1,0},{0,0}}]})
    ).

build_wall_test() ->
    Walls = [{{0,0},{1,0}},{{0,0},{0,1}},{{-1,0},{0,0}}],
    {W, H, NewWalls} = rooms:build_wall({1, 1, Walls}),
    io:fwrite("Testing with grid:~n"),
    io:fwrite("~p~n", [{W, H, Walls}]),
    ?assertEqual(
        {1,1, [{{-1,0},{0,0}},{{0,-1},{0,0}},{{0,0},{0,1}},{{0,0},{1,0}}]},
        {W, H, lists:sort(NewWalls)}
    ).
