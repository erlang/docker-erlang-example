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

    Dispatch = cowboy_router:compile(
                 [
                  {'_', [{"/[:counter_name]", dockerwatch_handler, []}]}
                 ]),

    CowConfig = #{ env => #{ dispatch => Dispatch },
                   metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                   stream_handlers => [cowboy_metrics_h, cowboy_stream_h] },

    HTTPS = ranch:child_spec(
              cowboy_https, 100, ranch_ssl,
              [{port, 8443},
               {cacertfile, filename:join(CertsDir, "dockerwatch-ca.pem")},
               {certfile, filename:join(CertsDir, "dockerwatch-server.pem")},
               {keyfile, filename:join(CertsDir, "dockerwatch-server.key")}],
              cowboy_tls,
              CowConfig),

    HTTP = ranch:child_spec(
             cowboy_http, 100, ranch_tcp,
             [{port, 8080}],
             cowboy_clear,
             CowConfig),

    PromConfig =
        #{ env => #{ dispatch =>
                         cowboy_router:compile(
                           [{'_', [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}]) }
         },

    Prometheus = ranch:child_spec(
                   cowboy_prometheus, 100, ranch_tcp,
                   [{port, 9000}],
                   cowboy_clear,
                   PromConfig),

    Counter = {dockerwatch, {dockerwatch, start_link, []},
               permanent, 5000, worker, [dockerwatch]},

    Procs = [Counter, HTTP, HTTPS, Prometheus],

    {ok, {{one_for_one, 10, 10}, Procs}}.
