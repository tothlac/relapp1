%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_srv).

-behaviour(gen_server).

-record(state, {
    id  :: non_neg_integer(),
    description_id :: non_neg_integer(),
    name = undefined :: undefined | binary()
}).

-export([start_link/0,
         test/1]).

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

init([]) ->
    {ok, #state{id = 0,
                description_id = 1,
                name = <<"name">>}}.

handle_call({test, Arg}, _From, State) ->
    Ret = relapp_m1:test(Arg),
    {reply, Ret, State};
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
