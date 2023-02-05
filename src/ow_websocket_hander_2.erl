-module(ow_websocket_hander_2).
-behaviour(cowboy_websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
}).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    queue_check(),
	{[], State}.

websocket_handle(_Data, State) ->
	{[], State}.

websocket_info(check, State) ->
    JSON = jsx:encode(ow_fetched_data_ets:tables()),
    queue_check(),
	{[{text, JSON}], State};
websocket_info(_Info, State) ->
	{[], State}.

terminate(_Reason, _Req, _State) ->
    ok.

queue_check() ->
    erlang:send_after(200, self(), check).