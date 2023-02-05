-module(ow_table_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Table Viewer".

fetch() ->
    erlang:error(not_implemented).