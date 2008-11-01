-module(gserver).

-export([start_link/0]).
-export([list_games/0, list_rooms/1]).
-export([to_game_name/1, to_room_name/2, to_room_atom/2, to_user_name/1, to_user_atom/1]).

%%--------------------------------------------------------------------

start_link() ->
    room:start_link("Tictactoe", "Beginner"),
    lobby:start_link().

%%--------------------------------------------------------------------

list_games() ->
    Atoms = registered(),
    Names = [gserver:to_game_name(A) || A <- Atoms],
    NonBlankNames = [N || N <- Names, N =/= ""],
    lists:usort(NonBlankNames).

list_rooms(GameName) ->
    Atoms = registered(),
    Names = [gserver:to_room_name(A, GameName) || A <- Atoms],
    [N || N <- Names, N =/= ""].

%%--------------------------------------------------------------------

to_game_name(Registered) ->
    String = atom_to_list(Registered),
    {ok, L} = regexp:split(String, "/"),
    if
        length(L) == 3 ->
            [Games, GameName, _] = L,
            if
                Games == "games" -> GameName;
                true -> ""
            end;
        true -> ""
    end.

%%--------------------------------------------------------------------

to_room_name(Registered, GameName) ->
    String = atom_to_list(Registered),
    {ok, L} = regexp:split(String, "/"),
    if
        length(L) == 3 ->
            [Games, GameName2, RoomName] = L,
            if
                (Games == "games") and (GameName2 == GameName) -> RoomName;
                true -> ""
            end;
        true -> ""
    end.

to_room_atom(GameName, RoomName) ->
    Path = "games/" ++ GameName ++ "/" ++ RoomName,
    list_to_atom(Path).

%%--------------------------------------------------------------------

to_user_name(UserAtom) ->
    String = atom_to_list(UserAtom),
    {ok, L} = regexp:split(String, "/"),
    if
        length(L) == 2 ->
            [Users, UserName] = L,
            if
                Users == "users" -> UserName;
                true -> ""
            end;
        true -> ""
    end.

to_user_atom(UserName) ->
    Path = "users/" ++ UserName,
    list_to_atom(Path).
