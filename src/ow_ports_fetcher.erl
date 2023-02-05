-module(ow_ports_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Ports".

fetch() ->
    erlang:error(not_implemented).