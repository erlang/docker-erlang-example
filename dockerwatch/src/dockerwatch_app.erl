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
    Dispatch = cowboy_router:compile([
	    {'_', [{"/", dockerwatch_handler, []}]}
	]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9898}], [
	    {env, [{dispatch, Dispatch}]}
	]),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.
