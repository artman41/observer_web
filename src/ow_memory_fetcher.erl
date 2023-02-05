-module(ow_memory_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Memory Allocators".

fetch() ->
    erlang:error(not_implemented).