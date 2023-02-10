PROJECT = observer_web
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += cowboy
DEPS += jsx

# CLONED_MODS += observer_wx
# CLONED_MODS += observer_perf_wx

ifeq ($(COWBOY_VSN),2)
dep_cowboy = $(pkg_cowboy_fetch) $(pkg_cowboy_repo) 2.9.0
else
dep_cowboy = $(pkg_cowboy_fetch) $(pkg_cowboy_repo) 1.1.2
endif

cloned_observer_wx = observer_wx cloned_observer_wx
cloned_observer_perf_wx = observer_perf_wx cloned_observer_perf_wx

ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

define prepatch_observer_wx.erl =
-compile(export_all).
endef

define prepatch_observer_perf_wx.erl =
-export([
	collect_data/2,
	calc_max/2
]).
-export([
	default_rec/1,
	rec_from_props/2,
	rec_to_props/2
]).
endef

define postpatch_observer_perf_wx.erl =
default_rec(paint) ->
	#paint{}.

rec_from_props(paint, Props) when is_list(Props) ->
	Default = default_rec(paint),
	list_to_tuple([paint | [
		case lists:keyfind(Field, 1, Props) of 
			{Field, V} -> 
				V; 
			false -> 
				proplists:get_value(Field, Default) 
		end || Field <- record_info(fields, paint)
	]]).

rec_to_props(paint, Rec = #paint{}) ->
	Fields = record_info(fields, paint),
	lists:zip(Fields, tl(tuple_to_list(Rec))).
endef

include erlang_vsn.mk
include clone.mk
include erlang.mk
include templates.mk

define dep_version
$(if $(wildcard $(DEPS_DIR)/$(1)/ebin/$(1).app),$(strip $(shell $(ERL) -pa $(DEPS_DIR)/$(1)/ebin -eval 'application:load($(1)), io:format("~s~n", [case application:get_key($(1), vsn) of {ok, Vsn} -> Vsn; _ -> "" end]), halt().')))
endef

dep_version_short = $(shell echo '$(call dep_version,$(1))' | sed 's|\.[0-9]*$$||g')

ERLC_OPTS += -DCOWBOY_VSN=$(call dep_version_short,cowboy)

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'
SHELL_OPTS += -sname $(PROJECT)