-module(ow_applications_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Applications".

fetch() ->
    erlang:error(not_implemented).