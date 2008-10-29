-module(gserver).

-export([start_link/0]).

start_link() ->
    room:start_link("Tictactoe", "Beginner"),
    lobby:start_link().
