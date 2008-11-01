%% Client API acts as the API for game rooms.

-module(client).

%-behaviour(gen_fsm).

-export([list_games/0, list_rooms/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% start_link() ->
%%     {ok,Pid} | ignore | {error, Error}
%% Description:
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(UserName) ->
    Atom = user_name_to_atom(UserName),
    gen_fsm:start_link({local, Atom}, ?MODULE, [], []).

list_games() ->
    Atoms = registered(),
    Names = [extract_game_name(A) || A <- Atoms],
    [N || N <- Names, N =/= ""].

list_rooms(GameName) ->
    Atoms = registered(),
    Names = [extract_room_name(A, GameName) || A <- Atoms],
    [N || N <- Names, N =/= ""].

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
    {ok, new, []}.

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
in_lobby(_Event, _From, State) ->
    Reply = new,
    {reply, Reply, play, State}.

in_room(_Event, _From, State) ->
    Reply = play,
    {reply, Reply, over, State}.

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
user_name_to_atom(UserName) ->
    Path = "users/" ++ UserName,
    list_to_atom(Path).

extract_game_name(RegisteredName) ->
    String = atom_to_list(RegisteredName),
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

extract_room_name(RegisteredName, GameName) ->
    String = atom_to_list(RegisteredName),
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
