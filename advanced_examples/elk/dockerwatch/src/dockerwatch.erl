%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch).

-export([start_link/0, all/0, create/1, get/1, increment/2, decrement/2]).

-include_lib("kernel/include/logger.hrl").

-type counter() :: binary().

-spec start_link() -> {ok, pid()}.
start_link() ->
    ?LOG_DEBUG("~p starting",[?MODULE]),
    {ok, spawn_link(fun() -> ets:new(?MODULE, [named_table, public]),
                    receive after infinity -> ok end end)}.

-spec all() -> [counter()].
all() ->
    ?LOG_DEBUG("~p all",[?MODULE]),
    ets:select(?MODULE, [{{'$1','_'},[],['$1']}]).

-spec create(counter()) -> ok | already_exists.
create(CounterName) ->
    case ets:insert_new(?MODULE, {CounterName, 0}) of
        true ->
            ?LOG_DEBUG("Counter ~s created",[CounterName]),
            ok;
        false ->
            ?LOG_DEBUG("Counter ~s already exists",[CounterName]),
            already_exists
    end.

-spec get(counter()) -> integer().
get(CounterName) ->
    ?LOG_DEBUG("Counter ~s, get",[CounterName]),
    ets:lookup_element(?MODULE, CounterName, 2).

-spec increment(counter(), integer()) -> ok.
increment(CounterName, Howmuch) ->
    ?LOG_DEBUG("Counter ~s, increment ~p",[CounterName,Howmuch]),
    _ = ets:update_counter(?MODULE, CounterName, [{2, Howmuch}]),
    ok.

-spec decrement(counter(), integer()) -> ok.
decrement(CounterName, Howmuch) ->
    ?LOG_DEBUG("Counter ~s, decrement ~p",[CounterName,Howmuch]),
    _ = ets:update_counter(?MODULE, CounterName, [{2, -1 * Howmuch}]),
    ok.
