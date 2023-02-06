CLONE_DIR ?= src/cloned

ERL ?= erl +A1 -noinput -boot no_dot_erlang -pa deps/*/ebin
DEPS_DIR ?= deps/

ifeq ($(V),)
verbose = @
endif

define get_module_compile_info
$(shell $(ERL) -eval \
	'io:format("~s~n", [proplists:get_value(source, $(1):module_info(compile))]), halt().' \
	| sed 's|.*/lib/\([^/]*\)/\(.*\)|\1 \2|g;s|.*/$(dir $(DEPS_DIR))\([^/]*\)/\(.*\)|\1 \2|g' \
)
endef

define lookup_module
$(strip $(if $(APP_$(1))$(LOC_$(1)),,
	$(eval $(1)=$(call get_module_compile_info,$(1)))
	$(eval APP_$(1) = $(word 1,$($(1))))
	$(eval LOC_$(1) = $(word 2,$($(1))))
	$(eval LIB_DIR_$(1) = $(shell $(ERL) -eval 'io:format("~s~n", [code:lib_dir($(APP_$(1)))]), halt().'))
))
endef

get_old_module = $(word 1,$(cloned_$(1)))
get_new_module = $(word 2,$(cloned_$(1)))
get_erl_file = $(CLONE_DIR)/$(1).erl

$(shell mkdir -p $(CLONE_DIR))

define do_clone
	$(if $(cloned_$(1)),,$(error No config for cloned_$(1))) \
	$(if $(wildcard $(call get_erl_file,$(call get_new_module,$(1)))),,\
		$(eval OLD_MOD = $(call get_old_module,$(1))) $(eval NEW_MOD = $(call get_new_module,$(1))) \
		$(eval $(if $(prepatch_$(1).erl),$(file > $(CLONE_DIR)/$(1).prepatch,$(value prepatch_$(1).erl)))) \
		$(eval $(if $(postpatch_$(1).erl),$$(file > $(CLONE_DIR)/$(1).postpatch,$$(value postpatch_$(1).erl)))) \
	) \
	echo " CLONE $(LIB_DIR_$(OLD_MOD))/$(LOC_$(OLD_MOD)) -> $(CLONE_DIR)/$(NEW_MOD).erl"; \
	sed 's|-module($(OLD_MOD))\.|-module($(NEW_MOD)).|g' $(LIB_DIR_$(OLD_MOD))/$(LOC_$(OLD_MOD)) > $(call get_erl_file,$(NEW_MOD)).tmp; \
	includes=`sed -n 's|-include("\(.*\)")\.|\1|p' $(LIB_DIR_$(OLD_MOD))/$(LOC_$(OLD_MOD))`; \
	for include in $$$$includes; do \
		file="$(LIB_DIR_$(OLD_MOD))/include/$$$$include"; \
		if test -f "$$$$file"; then \
			replacement_include="$(APP_$(OLD_MOD))/include/$$$$include"; \
		else \
			replacement_include="$(APP_$(OLD_MOD))/src/$$$$include"; \
		fi;\
		sed "s|-include(\"$$$$include\")|-include_lib(\"$$$$replacement_include\")|g" -i $(call get_erl_file,$(NEW_MOD)).tmp; \
	done; \
	SEEN_FUNCTION=false; \
	while IFS= read -r line; do \
		if test -f "$(CLONE_DIR)/$(1).prepatch"; then \
			if test -n "`echo "$$$$line" | grep '^[a-z]'`" && ! $$$$SEEN_FUNCTION; then \
				SEEN_FUNCTION=true; \
				echo "%% Prepatching START" >> $(call get_erl_file,$(NEW_MOD)); \
				cat "$(CLONE_DIR)/$(1).prepatch" >> $(call get_erl_file,$(NEW_MOD)); \
				echo "%% Prepatching END" >> $(call get_erl_file,$(NEW_MOD)); \
			fi; \
		fi; \
		echo "$$$$line" >> $(call get_erl_file,$(NEW_MOD)); \
	done < $(call get_erl_file,$(NEW_MOD)).tmp; \
	rm -f $(call get_erl_file,$(NEW_MOD)).tmp; \
	if test -f "$(CLONE_DIR)/$(1).postpatch"; then \
		echo "%% Postpatching START" >> $(call get_erl_file,$(NEW_MOD)); \
		cat "$(CLONE_DIR)/$(1).postpatch" >> $(call get_erl_file,$(NEW_MOD)); \
		echo "%% Postpatching END" >> $(call get_erl_file,$(NEW_MOD)); \
	fi; \
	rm -f "$(CLONE_DIR)/$(1).prepatch" "$(CLONE_DIR)/$(1).postpatch"
endef

$(strip $(foreach cloned,$(CLONED_MODS),$(call lookup_module,$(cloned))))

$(CLONE_DIR):
	$(verbose) mkdir -p $(CLONE_DIR);

define cloned_target
$(call get_erl_file,$(call get_new_module,$(1))):: $(CLONE_DIR)
	$(verbose) $(call do_clone,$(1))

.PHONY: $(1)
$(1):: $(call get_erl_file,$(call get_new_module,$(1)))
endef

$(foreach cloned,$(CLONED_MODS),$(eval $(call cloned_target,$(cloned))))

app:: $(foreach cloned,$(CLONED_MODS),$(cloned))

clean::
	rm -rf src/cloned;