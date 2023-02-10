TMP_DIR ?= $(if $(ERLANG_MK_TMP),$(ERLANG_MK_TMP),$(CURDIR)/.tmp/)
VSN_FILE = $(TMP_DIR)/erlang.vsn

ERLANG_VSN = $(shell erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt(0).')

ifeq ($(wildcard $(TMP_DIR)),)
$(shell mkdir -p $(TMP_DIR))
endif

ifeq ($(wildcard $(VSN_FILE)),)
$(file > $(VSN_FILE),LAST_VSN = $(ERLANG_VSN))
else
include $(VSN_FILE)
ifneq ($(wildcard ebin/*),)
ifneq ($(ERLANG_VSN),$(LAST_VSN))
$(error App was last compiled with OTP $(LAST_VSN), currently running OTP $(ERLANG_VSN)! Consider cleaning ebin/)
endif
endif
endif

clean::
	rm -f $(VSN_FILE)