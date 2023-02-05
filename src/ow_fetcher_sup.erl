-module(ow_fetcher_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        ow_fetcher_backend:child_spec(ow_system_fetcher),
        ow_fetcher_backend:child_spec(ow_load_fetcher),
        ow_fetcher_backend:child_spec(ow_memory_fetcher),
        ow_fetcher_backend:child_spec(ow_applications_fetcher),
        ow_fetcher_backend:child_spec(ow_processes_fetcher),
        ow_fetcher_backend:child_spec(ow_ports_fetcher),
        ow_fetcher_backend:child_spec(ow_sockets_fetcher),
        ow_fetcher_backend:child_spec(ow_table_fetcher),
        ow_fetcher_backend:child_spec(ow_trace_fetcher)
    ],
    {ok, {SupFlags, Children}}.
