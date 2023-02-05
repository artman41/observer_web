-module(observer_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/observer", cowboy_static, {priv_file, observer_web, "www/html/index.html"}},
            {"/observer/ws", ws_handler(), []},
            {"/observer/3rdparty/[...]", cowboy_static, {priv_dir, observer_web, "www/3rdparty/", [{mimetypes, cow_mimetypes, all}]}},
            {"/observer/js/[...]", cowboy_static, {priv_dir, observer_web, "www/js/", [{mimetypes, cow_mimetypes, all}]}},
            {"/observer/css/[...]", cowboy_static, {priv_dir, observer_web, "www/css/", [{mimetypes, cow_mimetypes, all}]}},
            {"/observer/[...]", cowboy_static, {priv_dir, observer_web, "www/html/", [{mimetypes, cow_mimetypes, all}]}}
		]}
	]),
    Children = [
        #{
            id => fetched_data_ets,
            start => {ow_fetched_data_ets, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        #{
            id => ticker_srv,
            start => {ow_ticker_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        #{
            id => fetcher_sup,
            start => {ow_fetcher_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor
        },
        cowboy_spec(observer_web, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}])
    ],
    error_logger:info_msg("Children: ~p~n", [Children]),
    {ok, {SupFlags, Children}}.

-ifdef(COWBOY_VSN).
cowboy_spec(Ref, NbAcceptors, TransOpts, ProtoOpts) when is_list(TransOpts) andalso is_list(ProtoOpts) ->
    case ?COWBOY_VSN of
        Vsn when Vsn >= 2.0 ->
            ProtoOptsMap = maps:from_list([{K, maps:from_list(V)} || {K,V} <- ProtoOpts]),
            ranch:child_spec(Ref, NbAcceptors, ranch_tcp, TransOpts, cowboy_clear, ProtoOptsMap);
        Vsn when Vsn >= 1.0 ->
            ranch:child_spec(Ref, NbAcceptors, ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts);
        _ ->
            erlang:error({unknown_cowboy_vsn, ?COWBOY_VSN})
    end.

ws_handler() ->
    case ?COWBOY_VSN of
        Vsn when Vsn >= 2.0 ->
            ow_websocket_hander_2;
        Vsn when Vsn >= 1.0 ->
            ow_websocket_hander_1;
        Vsn when Vsn >= 0 ->
            ow_websocket_hander_1;
        _ ->
            erlang:error({unknown_cowboy_vsn, ?COWBOY_VSN})
    end.
-else.
-error(no_cowboy_vsn_defined).
-endif.