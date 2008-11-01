-module(tictactoe).

-behaviour(gen_fsm).

%% API
-export([list_games/0, list_rooms/1]).
-export([enter/3, leave/3, chat/3]).
-export([config/3, join/2, unjoin/2, play/3]).
-export([start_link/1]).

%% gen_fsm callbacks
-export([
    init/1,
    joinable/3,
    playing/3,
    over/3,
    handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%====================================================================
%% API
%%====================================================================
list_games() ->
    Atoms = registered(),
    Names = [to_game_name(A) || A <- Atoms],
    NonBlankNames = [N || N <- Names, N =/= ""],
    lists:usort(NonBlankNames).

list_rooms(GameName) ->
    Atoms = registered(),
    Names = [to_room_name(A, GameName) || A <- Atoms],
    [N || N <- Names, N =/= ""].

enter(GameName, RoomName, UserName) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_all_state_event(Atom, {enter, UserName}).

leave(GameName, RoomName, UserName) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_all_state_event(Atom, {leave, UserName}).

chat(GameName, RoomName, Message) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_all_state_event(Atom, {chat, Message}).

config(GameName, RoomName, Config) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_event(Atom, {config, Config}).

join(GameName, RoomName) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_event(Atom, join).

unjoin(GameName, RoomName) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_event(Atom, unjoin).

play(GameName, RoomName, Data) ->
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:sync_send_event(Atom, {play, Data}).

%%--------------------------------------------------------------------
%% Function:
%% start_link() ->
%%     {ok,Pid} | ignore | {error, Error}
%% Description:
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(RoomName) ->
    GameName = atom_to_list(?MODULE),
    Atom = to_room_atom(GameName, RoomName),
    gen_fsm:start_link({local, Atom}, ?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% init(Args) ->
%%     {ok, StateName, State} |
%%     {ok, StateName, State, Timeout} |
%%     ignore |
%%     {stop, StopReason}
%% Description:
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([]) ->
    UserAtoms = [],
    Board = [-1, -1, -1, -1, -1, -1, -1, -1, -1],
    {ok, new, {UserAtoms, Board}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) ->
%%     {next_state, NextStateName, NextState} |
%%     {next_state, NextStateName, NextState, Timeout} |
%%     {reply, Reply, NextStateName, NextState} |
%%     {reply, Reply, NextStateName, NextState, Timeout} |
%%     {stop, Reason, NewState} |
%%     {stop, Reason, Reply, NewState}
%% Description:
%% There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
joinable(_Event, _From, State) ->
    Reply = new,
    {reply, Reply, play, State}.

playing(_Event, _From, State) ->
    Reply = play,
    {reply, Reply, over, State}.

over(_Event, _From, State) ->
    Reply = over,
    {reply, Reply, new, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) ->
%%     {next_state, NextStateName, NextState} |
%%     {next_state, NextStateName, NextState, Timeout} |
%%     {stop, Reason, NewState}
%% Description:
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName, State) ->
%%     {next_state, NextStateName, NextState} |
%%     {next_state, NextStateName, NextState, Timeout} |
%%     {reply, Reply, NextStateName, NextState} |
%%     {reply, Reply, NextStateName, NextState, Timeout} |
%%     {stop, Reason, NewState} |
%%     {stop, Reason, Reply, NewState}
%% Description:
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event({enter, UserName}, _From, StateName, {UserAtoms, Board}) ->
    UserAtom = to_user_atom(UserName),
    AlreadyEntered = lists:any(fun(X) -> X == UserAtom end, UserAtoms),
    if
        AlreadyEntered ->
            Reply = {error, {already_entered}},
            {reply, Reply, StateName, {UserAtoms, Board}};
        true ->
            UserAtoms2 = [UserAtom | UserAtoms],
            UserNames = [to_user_name(A) || A <- UserAtoms],
            Reply = {UserNames, Board},
            {reply, Reply, StateName, {UserAtoms2, Board}}
    end;

handle_sync_event({leave, UserName}, _From, StateName, {UserAtoms, Board}) ->
    UserAtoms2 = lists:delete(to_user_atom(UserName), UserAtoms),
    Reply = ok,
    {reply, Reply, StateName, {UserAtoms2, Board}};

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info, StateName, State) ->
%%     {next_state, NextStateName, NextState} |
%%     {next_state, NextStateName, NextState, Timeout} |
%%     {stop, Reason, NewState}
%% Description:
%% This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% terminate(Reason, StateName, State) ->
%%     void()
%% Description:
%% This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) ->
%%     {ok, StateName, NewState}
%% Description:
%% Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
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
