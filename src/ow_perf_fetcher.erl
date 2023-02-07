-module(ow_perf_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Load Charts".

fetch() ->
    IO = erlang:statistics(io),
    MemoryStats = try erlang:memory() catch _:_ -> [] end,
    
    [
        {time_us, erlang:system_time(micro_seconds)},
        {io, tuple_to_list(IO)},
        {memory, MemoryStats}
    ].