%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_srv).

-behaviour(gen_server).

-record(state, {
    id  :: non_neg_integer(),
    name = undefined :: undefined | binary(),
    extra = undefined :: undefined | {atom(), non_neg_integer()}
}).
%% declare the record that holds the state for this gen_server
-state_record(state).

-export([start_link/0,
         test/1,
         get_extra/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

test(undefined) -> ok;
test(Arg) ->
    gen_server:call(?MODULE, {test, Arg}).

get_extra() ->
    gen_server:call(?MODULE, get_extra).

init([]) ->
    {ok, #state{id = 0,
                name = <<"name">>}}.

handle_call({test, Arg}, _From, State) ->
    Ret = relapp_m1:test(Arg),
    {reply, Ret, State};
handle_call(get_extra, _From, State) ->
    {reply, State#state.extra, State};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change("1.0.9", State, _Extra) ->
    {ok, State};
code_change({down, "1.0.9"}, State, _Extra) ->
    {ok, State};
code_change({down, _OldVsn}, State0, Extra) ->
    %% since the state record has already been
    %% converted behind the scenes,
    %% the State var is already the new one, obtain the
    %% old state from the extra variable
    OldState = proplists:get_value(old_state, Extra),
    %% extract the extra argument
    {_, DescriptionId} = erlang:element(4, OldState),
    %% and set it in the new state
    State = erlang:setelement(3, State0, DescriptionId),
    {ok, State};
code_change(_OldVsn, State, Extra) ->
    %% since the state record has already been
    %% converted behind the scenes,
    %% the State var is already the new one, obtain the
    %% old state from the extra variable
    OldState = proplists:get_value(old_state, Extra),
    %% extract the value of the description id field
    %% from the old state
    DescriptionId = erlang:element(3, OldState),
    Arg = proplists:get_value(arg, Extra),
    ExtraArg = {Arg, DescriptionId},
    {ok, State#state{extra = ExtraArg}}.
