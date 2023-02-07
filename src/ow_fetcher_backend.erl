-module(ow_fetcher_backend).

-export([child_spec/1, start_link/1]).
-export([init/1]).

-callback init() -> no_return().
-callback name() -> string().
-callback fetch() -> proplists:proplist().

-optional_callbacks([name/0]).

child_spec(Module) when is_atom(Module) ->
    #{
        id => {?MODULE, Module},
        start => {?MODULE, start_link, [Module]},
        restart => permanent,
        shutdown => 2000,
        type => worker
    }.

start_link(Module) when is_atom(Module) ->
    case load_module(Module) andalso erlang:function_exported(Module, fetch, 0) of
        true ->
            proc_lib:start_link(?MODULE, init, [Module]);
        false ->
            {error, bad_callback_module}
    end.

init(CallbackMod) ->
    catch CallbackMod:init(),
    Name = 
        try CallbackMod:name()
        catch error:undef ->
            atom_to_list(CallbackMod)
        end,
    ow_ticker_srv:add(self()),
    proc_lib:init_ack({ok, self()}),
    loop(iolist_to_binary(Name), CallbackMod).

%% internal

loop(Name, CallbackMod) ->
    receive
        tick ->
            Ret = 
                try [{type, data} | CallbackMod:fetch()]
                catch E:R ->
                    [{type, exception}, {class, E}, {reason, R}]
                end,
            ow_fetched_data_ets:insert(Name, Ret)
    end,
    loop(Name, CallbackMod).

load_module(Mod) when is_atom(Mod) ->
    case code:load_file(Mod) of
        {module, Mod} ->
            true;
        _ ->
            false
    end.