undefine tpl_supervisor
undefine tpl_gen_server
undefine tpl_module
undefine tpl_cowboy_http
undefine tpl_gen_fsm
undefine tpl_gen_statem
undefine tpl_cowboy_loop
undefine tpl_cowboy_rest
undefine tpl_cowboy_ws
undefine tpl_ranch_protocol

__NOOP__=
__COMMA__=,
__SPACE__=$(__NOOP__) $(__NOOP__)

define tpl_application
-module($(n)).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    $(if $(sup),$(sup):start_link(),{ok$(__COMMA__)self()}).

stop(_State) ->
    ok.
endef

define tpl_supervisor
-module($(n)).
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
    Children = [
        % #{
        %     id => my_child,
        %     start => {my_mod, my_func, []},
        %     restart => permanent,
        %     shutdown => 2000,
        %     type => worker
        % }
    ],
    {ok, {SupFlags, Children}}.
endef

define tpl_gen_server
-module($(n)).
-behaviour(gen_server).

%% API.
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
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
endef

define tpl_module
-module($(n)).
-export([]).
endef

define tpl_cowboy_http
-module($(n)).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
endef

define tpl_gen_fsm
-module($(n)).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([state_name/2]).
-export([handle_event/3]).
-export([state_name/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%% gen_fsm.

init([]) ->
    {ok, state_name, #state{}}.

state_name(_Event, StateData) ->
    {next_state, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

state_name(_Event, _From, StateData) ->
    {reply, ignored, state_name, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
endef

define tpl_gen_statem
-module($(n)).
-behaviour(gen_statem).

%% API.
-export([start_link/0]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([state_name/3]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%% gen_statem.

callback_mode() ->
    state_functions.

init([]) ->
    {ok, state_name, #state{}}.

state_name(_EventType, _EventData, StateData) ->
    {next_state, state_name, StateData}.

handle_event(_EventType, _EventData, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
endef

define tpl_cowboy_loop
-module($(n)).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {loop, Req, #state{}, 5000, hibernate}.

info(_Info, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.
endef

define tpl_cowboy_rest
-module($(n)).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
    {<<"<html><body>This is REST!</body></html>">>, Req, State}.
endef

define tpl_cowboy_ws
-module($(n)).
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
    {ok, Req2, #state{}}.

websocket_handle({text, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
endef

define tpl_ranch_protocol
-module($(n)).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-type opts() :: [].
-export_type([opts/0]).

-record(state, {
    socket :: inet:socket(),
    transport :: module()
}).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, _Opts) ->
    ok = ranch:accept_ack(Ref),
    loop(#state{socket=Socket, transport=Transport}).

loop(State) ->
    loop(State).
endef

define tpl_config
-module($(n)).

-export([get_thing/0]).

get_thing() ->
    get_value(thing).

%% internal

get_value(Key) ->
    application:get_env($(PROJECT), Key).

get_value(Key, Default) ->
    case get_value(Key) of
        undefined -> Default;
        {ok, V} -> V
    end.
endef

define tpl_tab
-module($(n)).

-export([start_link/0]).
-export([init/0]).
-export([insert/2, lookup/1]).

-define(TAB, ?MODULE).

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
endef

define tpl_registry
-module($(n)).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([all/0]).
-export([whereis_name/1, register_name/2, unregister_name/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(state, {
}).

%% API.

all() ->
    ets:tab2list(?TABLE).

-spec whereis_name(Name :: any()) -> pid() | undefined.
whereis_name(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Pid}] ->
            Pid;
        _ ->
            undefined
    end.

-spec register_name(Name :: any(), pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?SERVER, {register, {Name, Pid}}).

-spec unregister_name(Name :: any()) -> true.
unregister_name(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    erlang:process_flag(trap_exit, true),
    ets:new(?TABLE, [protected, named_table]),
    {ok, #state{}}.

handle_call({register, KV}, _From, State) ->
    Ret = 
        case ets:insert_new(?TABLE, KV) of
            true -> yes;
            false -> no
        end,
    {reply, Ret, State};
handle_call({unregister, Name}, _From, State) ->
    true = ets:delete(?TABLE, Name),
    {reply, true, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_all_registered_processes(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% INTERNAL
stop_all_registered_processes() ->
    stop_all_registered_processes_(ets:first(?TABLE)).

stop_all_registered_processes_('$$end_of_table') ->
    ok;
stop_all_registered_processes_(Name) ->
    Pid = ets:lookup_element(?TABLE, Name, 2),
    erlang:exit(Pid, {shutdown, registry_dead}),
    ets:delete(?TABLE, Name),
    stop_all_registered_processes_(ets:first(?TABLE)).
endef

define tpl_proc_lib
-module($(n)).

-export([start_link/0]).
-export([init/0]).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init() ->
    proc_lib:init_ack({ok, self()}),
    loop().

%% internal

loop() ->
    receive
    after infinity ->
        ok
    end.
endef

define tpl_gen_event
-module(ow_ticker_event).
-behaviour(gen_event).

-export([add_handler/0, delete_handler/1]).
-export([raise_event/1]).
-export([start_link/0]). 
-export([
    init/1, 
    handle_event/2, 
    handle_call/2, 
    handle_info/2, 
    code_change/3,
    terminate/2
]).

-define(SERVER, ?MODULE).

-record(state, {}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

delete_handler(Reason) ->
    gen_event:delete_handler(?SERVER, ?MODULE, Reason).

raise_event(Event) ->
    gen_event:notify(?SERVER, Event).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

init([]) ->
    {ok, #state{}}.
 
handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
endef