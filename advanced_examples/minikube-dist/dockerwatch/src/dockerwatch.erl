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
    ok = mnesia:wait_for_tables([?MODULE], 15000),
    ignore.

-spec all() -> [counter()].
all() ->
    case mnesia:transaction(
           fun() ->
                   mnesia:select(?MODULE, [{{'$1','_'},[],['$1']}])
           end) of
        {atomic, Res} ->
            Res
    end.

-spec create(counter()) -> ok | already_exists.
create(CounterName) ->
    case mnesia:transaction(
           fun() ->
                   case mnesia:read(?MODULE, CounterName) of
                       [] ->
                           mnesia:write({?MODULE,CounterName,0}),
                           ok;
                       _Else ->
                           already_exists
                   end
           end) of
        {atomic, Res} ->
            Res
    end.

-spec get(counter()) -> integer().
get(CounterName) ->
    case mnesia:transaction(
           fun() ->
                   mnesia:read(?MODULE, CounterName)
           end) of
        {atomic,[{?MODULE, _, Cnt}]} ->
            Cnt
    end.

-spec increment(counter(), integer()) -> ok.
increment(CounterName, Howmuch) ->
    case mnesia:transaction(
           fun() ->
                   [{?MODULE, _, Cnt}] = mnesia:read(?MODULE, CounterName),
                   mnesia:write({?MODULE, CounterName, Cnt + Howmuch})
           end) of
        {atomic, ok} ->
            ok
    end.

-spec decrement(counter(), integer()) -> ok.
decrement(CounterName, Howmuch) ->
    increment(CounterName, -1 * Howmuch).
