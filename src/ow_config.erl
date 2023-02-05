-module(ow_config).

-export([get_tick_rate/0, get_tabs/0]).

get_tick_rate() ->
    get_value(tick_rate, 200).

get_tabs() ->
    get_value(tabs, [
        iolist_to_binary(ow_system_fetcher:name()),
        iolist_to_binary(ow_load_fetcher:name()),
        iolist_to_binary(ow_memory_fetcher:name()),
        iolist_to_binary(ow_applications_fetcher:name()),
        iolist_to_binary(ow_processes_fetcher:name()),
        iolist_to_binary(ow_ports_fetcher:name()),
        iolist_to_binary(ow_sockets_fetcher:name()),
        iolist_to_binary(ow_table_fetcher:name()),
        iolist_to_binary(ow_trace_fetcher:name())
    ]).

%% internal

get_value(Key) ->
    application:get_env(observer_web, Key).

get_value(Key, Default) ->
    case get_value(Key) of
        undefined -> Default;
        {ok, V} -> V
    end.
