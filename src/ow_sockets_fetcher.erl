-module(ow_sockets_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Sockets".

fetch() ->
    erlang:error(not_implemented).