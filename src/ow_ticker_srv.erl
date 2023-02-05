-module(ow_ticker_srv).
-behaviour(gen_server).

%% API.
-export([add/1, remove/1]).
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    pids :: list()
}).

%% API.

add(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {add, Pid}).

remove(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {remove, Pid}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    queue_tick(),
    {ok, #state{
        pids = []
    }}.

handle_call({add, Pid}, _From, State0) when is_pid(Pid) ->
    State1 = 
        case get({'$monitor', Pid}) of
            undefined ->
                put({'$monitor', Pid}, monitor(process, Pid)),
                State0#state{pids = [Pid | State0#state.pids]};
            _ ->
                State0
        end,
    {reply, ok, State1};
handle_call({remove, Pid}, _From, State0) when is_pid(Pid) ->
    State1 = 
        case erase({'$monitor', Pid}) of
            undefined ->
                State0;
            MonRef ->
                demonitor(MonRef),
                State0#state{pids = State0#state.pids -- [Pid]}
        end,
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {noreply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    [Pid ! tick || Pid <- State#state.pids],
    queue_tick(),
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    erase({'$monitor', Pid}),
    {noreply, State#state{pids = State#state.pids -- [Pid]}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

queue_tick() ->
    erlang:send_after(ow_config:get_tick_rate(), self(), tick).