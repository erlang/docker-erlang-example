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

-include_lib("kernel/include/logger.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    CertsDir = "/etc/ssl/certs/",

    Dispatch = cowboy_router:compile([
	    {'_', [{"/[:counter_name]", dockerwatch_handler, []}]}
	]),

    HTTPS = ranch:child_spec(
              cowboy_https, 100, ranch_ssl,
              [{port, 8443},
               {cacertfile, filename:join(CertsDir, "dockerwatch-ca.pem")},
               {certfile, filename:join(CertsDir, "dockerwatch-server.pem")},
               {keyfile, filename:join(CertsDir, "dockerwatch-server.key")}],
              cowboy_tls,
              #{env=>#{dispatch=>Dispatch},
                metrics_callback=>log_fun(),
                stream_handlers => [cowboy_metrics_h,cowboy_stream_h]}),

    HTTP = ranch:child_spec(
             cowboy_http, 100, ranch_tcp,
             [{port, 8080}],
             cowboy_clear,
             #{env=>#{dispatch=>Dispatch},
               metrics_callback=>log_fun(),
               stream_handlers => [cowboy_metrics_h,cowboy_stream_h]}),

    Counter = {dockerwatch, {dockerwatch, start_link, []},
               permanent, 5000, worker, [dockerwatch]},

    Procs = [Counter, HTTP, HTTPS],

    {ok, {{one_for_one, 10, 10}, Procs}}.

log_fun() ->
    fun(#{resp_status:=RS}=M) when RS>=100, RS<200 ->
            do_log(M,"Info");
       (#{resp_status:=RS}=M) when RS>=200, RS<300 ->
            do_log(M,"OK");
       (#{resp_status:=RS}=M) when RS>=300, RS<400 ->
            do_log(M,"Redirect");
       (#{resp_status:=RS}=M) when RS>=400, RS<500 ->
            do_log(M,"Client error");
       (#{resp_status:=RS}=M) when RS>=500 ->
            do_log(M,"Server error")
      end.

do_log(#{req:=#{scheme:=S,method:=M,path:=P},resp_status:=RS},_What) ->
    ?LOG_DEBUG("scheme=~s, method=~s, path=~s, resp_status=~p",[S,M,P,RS]);
do_log(#{reason:=Reason,resp_status:=RS},_What) ->
    ?LOG_DEBUG("reason=~p, resp_status=~p",[Reason,RS]).
