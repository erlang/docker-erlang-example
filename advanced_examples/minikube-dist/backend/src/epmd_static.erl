%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(epmd_static).

-export([start_link/0, register_node/2, register_node/3,
         port_please/2, address_please/3]).
%% API.

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet_tcp).
register_node(_Name, _Port, _Driver) ->
    {ok, 0}.

port_please(_Name, _Host) ->
    {port, 12345, 5}.

address_please(Name, Host, AddressFamily) ->
    erl_epmd:address_please(Name, Host, AddressFamily).
