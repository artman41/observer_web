-module(ow_load_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Load Charts".

fetch() ->
    erlang:error(not_implemented).