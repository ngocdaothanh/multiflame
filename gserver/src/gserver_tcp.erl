-module(gserver_tcp).
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket, []).

accept(LSocket, Sockets) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv(Socket, self()) end),
    accept(LSocket, [Socket | Sockets]).

recv(Socket, ParentPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %gen_tcp:send(Socket, Data),
            io:format("~p~n", [Data]),
            io:format("~p~n", [amf3:decode_all(Data)]),
            recv(Socket, ParentPid);
        {error, closed} ->
            gen_server:call(gserver_server, {closed, Socket}),
            ok
    end.
