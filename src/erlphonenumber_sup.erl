-module(erlphonenumber_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1000000,
                  period => 1 },
    ChildSpecs = [#{ id => erlphonenumber,
                     start => {erlphonenumber, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [erlphonenumber] }],
    {ok, {SupFlags, ChildSpecs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
