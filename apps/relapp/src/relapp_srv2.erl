%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_srv2).

-behaviour(gen_server).

-record(state, {
    id :: non_neg_integer()
}).
%% declare the record that holds the state for this gen_server
-state_record(state).

-export([start_link/0,
         set_state/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_state(State) ->
    ok = gen_server:call(?MODULE, {set_state, State}).

init([]) ->
    {ok, #state{id = 0}}.

handle_call({set_state, NewState}, _From, _State) ->
    {reply, ok, NewState};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
