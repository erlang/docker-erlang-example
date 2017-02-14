%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_handler.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([to_text/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[{<<"text/html">>, to_html},
      {<<"application/json">>, to_json},
      {<<"text/plain">>, to_text}
    ], Req, State}.

to_html(Req, State) ->
    Body = <<"<html>
    <head>
    <meta charset=\"utf-8\">
    <title>REST Hello World!</title>
    </head>
    <body>
    <p>REST Hello World as HTML!</p>
    </body>
    </html>">>,
    {Body, Req, State}.

to_json(Req, State) ->
    Ds = #{ <<"hello">> => <<"world">> },
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {jsone:encode(Ds), Req1, State}.

to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.
