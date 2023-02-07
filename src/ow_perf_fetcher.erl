-module(ow_perf_fetcher).
-behaviour(ow_fetcher_backend).
-export([
    init/0, 
    name/0, 
    fetch/0
]).

init() ->
    erlang:system_flag(scheduler_wall_time, ow_config:should_watch_scheduler()).

name() ->
    "Load Charts".

fetch() ->
    IO = erlang:statistics(io),
    MemoryStats = try erlang:memory() catch _:_ -> [] end,
    SchedulerWallTimeStats = 
        case erlang:statistics(scheduler_wall_time) of
            undefined -> 
                [];
            SchedulerWallTime ->
                [{Id, 100 * ActiveTime / TotalTime} || {Id, ActiveTime, TotalTime} <- SchedulerWallTime]
        end,
    
    [
        {time_us, erlang:system_time(micro_seconds)},
        {io, tuple_to_list(IO)},
        {memory, MemoryStats},
        {scheduler_wall_time, SchedulerWallTimeStats}
    ].