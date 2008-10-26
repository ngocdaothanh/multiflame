-module(flash_proxy).
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> proxy(Socket) end),
    accept(LSocket).

proxy(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %gen_tcp:send(Socket, Data),
            io:format("~p~n", [Data]),
            io:format("~p~n", [amf3:decode_all(Data)]),
            proxy(Socket);
        {error, closed} ->
            ok
    end.
