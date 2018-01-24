%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_srv2).

-behaviour(gen_server).

-record(state, {
    id :: non_neg_integer(),
    name = <<"">> :: binary(),
    description = <<"">> :: binary(),
    f = undefined :: undefined | function()
}).
%% declare the record that holds the state for this gen_server
-state_record(state).

-export([start_link/0,
         set_state/1,
         store_fun/1,
         call_stored_fun/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_state(undefined) -> {error, no_state};
set_state(State) ->
    ok = gen_server:call(?MODULE, {set_state, State}).

store_fun(Fun) ->
    gen_server:cast(?MODULE, {store_fun, Fun}).

call_stored_fun() ->
    gen_server:cast(?MODULE, call_stored_fun).

init([]) ->
    {ok, #state{id = 0,
                 name = <<"name">>,
                 description = <<"description">>}}.

handle_call({set_state, NewState}, _From, _State) ->
    {reply, ok, NewState};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast({store_fun, Fun}, State) ->
    {noreply, State#state{f = Fun}};
handle_cast(call_stored_fun,
            #state{f = Fun} = State) ->
    Fun(),
    {noreply, State};
handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    Attributes = relapp_srv2:module_info(attributes),
    Vsn = proplists:get_value(vsn, Attributes),
    io:format("code change from ~p(~p) to ~p(~p)",
        [OldVsn, State, Vsn, State]),
    {ok, State}.
