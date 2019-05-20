%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(backend_app).
-behaviour(application).

-export([start/2,stop/1]).
%% API.

start(_Type, _Args) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(dockerwatch,
                        [{disc_copies,[node()]},
                         {ram_copies,[]}]),
    {ok, self()}.

stop(_State) ->
    ok.
