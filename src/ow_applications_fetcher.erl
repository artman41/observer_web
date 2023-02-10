-module(ow_applications_fetcher).
-behaviour(ow_fetcher_backend).
-export([name/0, fetch/0]).

name() ->
    "Applications".

fetch() ->
    Apps = application:which_applications(),
    Pids = [Pid || {App, _, _} <- Apps, begin Pid = application_controller:get_master(App), Pid =/= undefined end],
    Workers = [begin 
        Worker = proc_lib:spawn(erlang, apply, [fun dig/1, [Pid]]),
        MonRef = monitor(process, Worker),
        {Worker, MonRef}
    end || Pid <- Pids],
    [Ret || {Pid, Ref} <- Workers, begin
        Ret = 
            receive
                {Pid, Return} ->
                    Return;
                {'DOWN', Ref, process, Pid, Reason} ->
                    ignore
            end,
        Ret =/= ignore
    end].

dig(Pid) ->
    [{dictionary, Dictionary}] = erlang:process_info(Pid, [dictionary]),
    dig(Pid, Dictionary).

dig(Pid, Dictionary) -> 
    ProcInfo = erlang:process_info(Pid, [registered_name, links]),
    {registered_name, RegisteredName} = lists:keyfind(registered_name, 1, ProcInfo),
    {links, Links} = lists:keyfind(links, 1, ProcInfo),
    Ancestors = proplists:get_value('$ancestors', Dictionary, []),
    Name =
        case RegisteredName =:= [] of
            true ->
                pid_to_list(Pid);
            false ->
                atom_to_list(RegisteredName)
        end,
    Children = [Link || Link <- (Links -- Ancestors), erlang:is_pid(Link)],
    Prop = 
        {pid_to_binary(Pid), [
            {name, list_to_binary(Name)},
            {children, lists:reverse(lists:foldl(
                fun(Child, Acc) -> 
                    [{dictionary, ChildDict}] = erlang:process_info(Child, [dictionary]),
                    case proplists:get_value('$ancestors', ChildDict, []) of 
                        [Pid | _] -> 
                            [dig(Child, ChildDict) | Acc];
                        _ -> 
                            Acc
                    end
                end, [], Children))},
            {links, lists:map(fun pid_to_binary/1, Links)},
            {ancestors, lists:map(fun pid_to_binary/1, Ancestors)}
        ]},
    hd(erlang:get('$ancestors')) ! {self(), Prop}.

pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).