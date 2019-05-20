%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch).

-export([start_link/0, all/0, create/1, get/1, increment/2, decrement/2]).

-type counter() :: binary().

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, spawn_link(fun() -> ets:new(?MODULE, [named_table, public]),
                    receive after infinity -> ok end end)}.

-spec all() -> [counter()].
all() ->
    ets:select(?MODULE, [{{'$1','_'},[],['$1']}]).

-spec create(counter()) -> ok | already_exists.
create(CounterName) ->
    case ets:insert_new(?MODULE, {CounterName, 0}) of
        true ->
            ok;
        false ->
            already_exists
    end.

-spec get(counter()) -> integer().
get(CounterName) ->
    ets:lookup_element(?MODULE, CounterName, 2).

-spec increment(counter(), integer()) -> ok.
increment(CounterName, Howmuch) ->
    _ = ets:update_counter(?MODULE, CounterName, [{2, Howmuch}]),
    ok.

-spec decrement(counter(), integer()) -> ok.
decrement(CounterName, Howmuch) ->
    _ = ets:update_counter(?MODULE, CounterName, [{2, -1 * Howmuch}]),
    ok.
