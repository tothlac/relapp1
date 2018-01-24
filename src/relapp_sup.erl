%%%-------------------------------------------------------------------
%% @doc relupapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relapp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         helper_method/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

helper_method() -> ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1},
            [{relapp_srv, {relapp_srv, start_link, []},
              transient, 5000, worker, [relapp_srv]},
             {relapp_srv2, {relapp_srv2, start_link, []},
              transient, 5000, worker, [relapp_srv2]},
             {relapp_app_sup, {relapp_app_sup, start_link, []},
              transient, 5000, supervisor, [relapp_app_sup]}
         ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
