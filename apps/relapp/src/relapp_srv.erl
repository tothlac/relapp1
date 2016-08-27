%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_srv).

-behaviour(gen_server).

-record(state, {
    id  :: non_neg_integer(),
    description_id :: non_neg_integer(),
    name = undefined :: undefined | binary(),
    extra = undefined :: undefined | binary()
}).

-export([start_link/0,
         test/1,
         set_description_id/1,
         get_description_id/0,
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

set_description_id(Arg) ->
    gen_server:call(?MODULE, {set_description_id, Arg}).

get_description_id() ->
    gen_server:call(?MODULE, get_description_id).

get_extra() ->
    gen_server:call(?MODULE, get_extra).

init([]) ->
    {ok, #state{id = 0,
                description_id = 1,
                name = <<"name">>}}.

handle_call({test, Arg}, _From, State) ->
    Ret = relapp_m1:test(Arg),
    {reply, Ret, State};
handle_call({set_description_id, Arg}, _From, State) ->
    {reply, ok,
     State#state{description_id = Arg}};
handle_call(get_description_id, _From,
            #state{description_id = DescriptionId} = State) ->
    {reply, {ok, DescriptionId}, State};
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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
