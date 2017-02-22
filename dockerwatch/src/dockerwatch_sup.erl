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
    CertsDir = "/etc/ssl/certs/",

    Dispatch = cowboy_router:compile([
	    {'_', [{"/", dockerwatch_handler, []}]}
	]),

    HTTPS = ranch:child_spec(
              cowboy_https, 100, ranch_ssl,
              [{port, 8443},
               {cacertfile, filename:join(CertsDir, "dockerwatch-ca.pem")},
               {certfile, filename:join(CertsDir, "dockerwatch-server.pem")},
               {keyfile, filename:join(CertsDir, "dockerwatch-server.key")}],
              cowboy_protocol,
              [{env, [{dispatch, Dispatch}]}]),

    HTTP = ranch:child_spec(
             cowboy_http, 100, ranch_tcp,
             [{port, 8080}],
             cowboy_protocol,
             [{env, [{dispatch, Dispatch}]}]),


    Procs = [HTTP, HTTPS],

    {ok, {{one_for_one, 10, 10}, Procs}}.
