-module(tictactoe).

-behaviour(gen_fsm).

%% API
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
    Atom = room_name_to_atom(RoomName),
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
    {ok, new, [-1, -1, -1, -1, -1, -1, -1, -1, -1]}.

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
room_name_to_atom(RoomName) ->
    GameName = atom_to_list(?MODULE),
    Path = "games/" ++ GameName ++ "/" ++ RoomName,
    list_to_atom(Path).
