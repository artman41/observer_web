-module(ow_system_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "System".

fetch() ->
    Data = observer_backend:sys_info(),

    Format = 
        [
            {<<"Info">>, [
                {<<"System and Architecture">>, [
                    {<<"System Version">>, otp_release},
                    {<<"ERTS Version">>, version},
                    {<<"Compiled for">>, system_architecture},
                    {<<"Emulator Wordsize">>, wordsize_external},
                    {<<"Process Wordsize">>, wordsize_internal},
                    {<<"SMP Support">>,  smp_support},
                    {<<"Thread Support">>,  threads},
                    {<<"Async thread pool size">>,  thread_pool_size}
                ]},
                {<<"CPU's and Threads">>, [
                    {<<"Logical CPU's">>, logical_processors},
                    {<<"Online Logical CPU's">>, logical_processors_online},
                    {<<"Available Logical CPU's">>, logical_processors_available},
                    {<<"Schedulers">>, schedulers},
                    {<<"Online schedulers">>, schedulers_online},
                    {<<"Available schedulers">>, schedulers_available}
                ]}
            ]},
            {<<"Stats">>, [
                {<<"Memory Usage">>, [
                    {<<"Total">>, total},
                    {<<"Processes">>, processes},
                    {<<"Atoms">>, atom},
                    {<<"Binaries">>, binary},
                    {<<"Code">>, code},
                    {<<"ETS">>, ets}
                ]},
                {<<"Statistics">>, [
                    {<<"Up time">>, uptime},
                    {<<"Max Processes">>, process_limit},
                    {<<"Processes">>, process_count},
                    {<<"Run Queue">>, run_queue},
                    {<<"IO Input">>,  io_input},
                    {<<"IO Output">>, io_output}
                ]}
            ]}
        ],
    format_data(Format, Data).

format_data([], _Proplist) ->
    [];
format_data([{GroupName, SubData}|Tail], Proplist) when is_list(SubData) ->
    [{GroupName, format_data(SubData, Proplist)} | format_data(Tail, Proplist)];
format_data([{Name, Key}|Tail], Proplist) when is_atom(Key) ->
    ValueStr = 
        iolist_to_binary(case proplists:get_value(Key, Proplist) of
            undefined -> "Undefined";
            Value when is_list(Value) -> Value;
            Value -> io_lib:format("~p", [Value])
        end),
    [{Name, ValueStr} | format_data(Tail, Proplist)].