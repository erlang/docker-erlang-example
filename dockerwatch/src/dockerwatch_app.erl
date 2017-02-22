%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_app).
-behaviour(application).

-export([start/2,stop/1]).
%% API.

start(_Type, _Args) ->
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.
