-module(key_value_server).
-behaviour(gen_server).

-export([ start/1, write/3, read/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2 ]).

start(DefaultValue) -> gen_server:start(key_value_server, DefaultValue, []).

write(Pid, Key, Value) -> gen_server:cast(Pid, { write, Key, Value }).

read(Pid, Key) -> gen_server:call(Pid, { read, Key }).

init(DefaultValue) -> { ok, { DefaultValue, [] } }.

handle_call({ read, Key }, _From, { DefaultValue, KeyValues }=State) ->
  { reply, find(Key, KeyValues, DefaultValue), State }.

handle_cast({ write, Key, Value }, { DefaultValue, KeyValues }) ->
  { noreply, { DefaultValue, [ { Key, Value }|KeyValues ] } }.

handle_info(_, State) ->
  { noreply, State }.

find(_Key, [], DefaultValue) -> DefaultValue;
find(Key, [ { Key, Value }|_ ], _DefaultValue) -> Value;
find(Key, [ _|T ], DefaultValue) -> find(Key, T, DefaultValue).
