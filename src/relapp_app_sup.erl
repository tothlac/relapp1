%%%-------------------------------------------------------------------
%% @doc
%% @end
%%%-------------------------------------------------------------------

-module(relapp_app_sup).

-behaviour(supervisor).
-behaviour(application).

%% API
-export([start_link/0,
         helper_method/0]).

%% Supervisor callbacks
-export([init/1]).
%% Application callbacks
-export([start/2,
         stop/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

helper_method() -> ok.

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    relapp_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1},
            []}}.

%%====================================================================
%% Internal functions
%%====================================================================
