-module(ow_processes_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Processes".

fetch() ->
    erlang:error(not_implemented).