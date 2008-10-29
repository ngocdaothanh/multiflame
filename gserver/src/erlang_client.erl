-module(erlang_client).

-export([enter_root/0]).

enter_root() ->
    Any = gen_server:call(lobby, {enter_root}),
    io:format("erlang client: ~p~n", [Any]),
    loop().

loop() ->
    ok.
