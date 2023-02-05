-module(ow_fetched_data_ets).

-export([start_link/0]).
-export([init/0]).
-export([all/0, tables/0, insert/2, lookup/1]).

-define(TAB, ?MODULE).

all() ->
    ets:tab2list(?TAB).

tables() ->
    [{Tab, Data} || Tab <- ow_config:get_tabs(), 
        begin 
            Data = 
                case lookup(Tab) of 
                    {ok, V} -> V; 
                    _ -> error 
                end, 
            Data =/= error 
        end].

insert(Key, Value) ->
    ets:insert(?TAB, {Key, Value}).

lookup(Key) ->
    case ets:lookup(?TAB, Key) of
        [] -> 
            {error, notfound};
        [{Key, Value}] ->
            {ok, Value}
    end.

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init() ->
    ets:new(?TAB, [public, named_table]),
    proc_lib:init_ack({ok, self()}),
    loop().

%% internal

loop() ->
    receive
    after infinity ->
        ok
    end.
