PROJECT = observer_web
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += cowboy
DEPS += jsx

ifeq ($(COWBOY_VSN),2)
dep_cowboy = $(pkg_cowboy_fetch) $(pkg_cowboy_repo) 2.9.0
else
dep_cowboy = $(pkg_cowboy_fetch) $(pkg_cowboy_repo) 1.1.2
endif

ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

include erlang.mk
include templates.mk

define dep_version
$(if $(wildcard $(DEPS_DIR)/$(1)/ebin/$(1).app),$(strip $(shell $(ERL) -pa $(DEPS_DIR)/$(1)/ebin -eval 'application:load($(1)), io:format("~s~n", [case application:get_key($(1), vsn) of {ok, Vsn} -> Vsn; _ -> "" end]), halt().')))
endef

dep_version_short = $(shell echo '$(call dep_version,$(1))' | sed 's|\.[0-9]*$$||g')

ERLC_OPTS += -DCOWBOY_VSN=$(call dep_version_short,cowboy)

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'