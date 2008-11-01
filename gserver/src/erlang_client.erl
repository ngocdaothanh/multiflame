-module(erlang_client).

-export([start/1]).

start(UserName) ->
    Pid = spawn_link(fun() -> loop(UserName) end),
    register(gserver:to_user_atom(UserName), Pid).

loop(UserName) ->
    receive
        {enter, OtherName} ->
            io:format("enter, ~p~n", [OtherName]),
            loop(UserName);
        {leave, OtherName} ->
            io:format("leave, ~p~n", [OtherName]),
            loop(UserName);
        {chat, OtherName, Message} ->
            io:format("chat, ~p: ~p~n", [OtherName, Message]),
            loop(UserName);
        _Any ->
            loop(UserName)
    end.
