%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_sup.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_sup).
-behaviour(supervisor).

-export([start_link/0,init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}}.
