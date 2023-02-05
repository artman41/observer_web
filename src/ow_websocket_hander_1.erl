-module(ow_websocket_hander_1).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
}).

init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    Req2 = cowboy_req:compact(Req),
    queue_check(),
    {ok, Req2, #state{}}.

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(check, Req, State) ->
    JSON = jsx:encode(ow_fetched_data_ets:tables()),
    queue_check(),
    {reply, {text, JSON}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

queue_check() ->
    erlang:send_after(200, self(), check).