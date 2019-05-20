-module(dockerwatch_filter).

-export([add_level_int/2]).

%% Add a field named level_int to the metadata of each log event. For
%% the formatter to insert in the message string when required by the
%% log target
add_level_int(#{level:=L,meta:=M}=E,_) ->
    E#{meta=>M#{level_int=>logger_config:level_to_int(L)}}.
