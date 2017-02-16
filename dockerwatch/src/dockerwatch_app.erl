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
    PrivDir = code:priv_dir(dockerwatch),
    Opts = [{port, 8443},
            {cacertfile, filename:join(PrivDir,"ssl/dockerwatch-ca.pem")},
            {certfile, filename:join(PrivDir,"ssl/dockerwatch-server.pem")},
            {keyfile, filename:join(PrivDir,"ssl/dockerwatch-server.key")}],
    {ok, _} = cowboy:start_https(https, 100, Opts, [{env, [{dispatch, Dispatch}]}]),
    dockerwatch_sup:start_link().

stop(_State) ->
    ok.
