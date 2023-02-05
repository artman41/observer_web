-module(ow_trace_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Trace Overview".

fetch() ->
    erlang:error(not_implemented).