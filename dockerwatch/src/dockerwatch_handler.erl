%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    dockerwatch_handler.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-10
%%

-module(dockerwatch_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([handle_post/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([to_text/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

%% Which HTTP methods are allowed
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% Which content types are accepted by POST/PUT requests
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_post}],
     Req, State}.

%% Handle the POST/PUT request
handle_post(Req, State) ->
    case cowboy_req:binding(counter_name, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Name, Req2} ->
            case cowboy_req:has_body(Req2) of
                true ->
                    {ok, Body, Req3} = cowboy_req:body(Req2),
                    Json = jsone:decode(Body),
                    ActionBin = maps:get(<<"action">>, Json, <<"increment">>),
                    Value = maps:get(<<"value">>, Json, 1),
                    Action = list_to_atom(binary_to_list(ActionBin)),
                    ok = dockerwatch:Action(Name, Value),
                    {true, Req3, State};
                false ->
                    ok = dockerwatch:create(Name),
                    {true, Req2, State}
            end
    end.

%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"text/html">>, to_html},
      {<<"application/json">>, to_json},
      {<<"text/plain">>, to_text}
    ], Req, State}.


%% Return counters/counter as json
to_json(Req, State) ->
    Resp = case cowboy_req:binding(counter_name, Req) of
               {undefined, _Req2} ->
                   dockerwatch:all();
               {Counter, _Req2} ->
                   #{ Counter => dockerwatch:get(Counter) }
           end,
    {jsone:encode(Resp), Req, State}.

%% Return counters/counter as plain text
to_text(Req, State) ->
    Resp = case cowboy_req:binding(counter_name, Req) of
               {undefined, _Req2} ->
                   [io_lib:format("~s~n",[Counter]) || Counter <- dockerwatch:all()];
               {Counter, _Req2} ->
                   io_lib:format("~p",[dockerwatch:get(Counter)])
           end,
    {Resp, Req, State}.

%% Return counters/counter as html
to_html(Req, State) ->
    Body = case cowboy_req:binding(counter_name, Req) of
               {undefined, _Req2} ->
                   Counters = dockerwatch:all(),
                   ["<ul>\n",
                    [io_lib:format("<li>~s</li>\n", [Counter]) || Counter <- Counters],
                    "</ul>\n"];
               {Counter, _Req2} ->
                   Value = dockerwatch:get(Counter),
                   io_lib:format("~s = ~p",[Counter, Value])
           end,
    {[html_head(),Body,html_tail()], Req, State}.

html_head() ->
    <<"<html>
    <head>
    <meta charset=\"utf-8\">
    <title>dockerwatch</title>
    </head>">>.
html_tail() ->
    <<"</body>
    </html>">>.
