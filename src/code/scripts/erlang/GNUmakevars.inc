PROJECT_NAME := Erlang
ARCHIVE_ROOT := $$HOME/Archive/Erlang
ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2


ERLANG_VERSION      := R9C-2
#ERLANG_ROOT        := /logiciels/public/otp_src_$(ERLANG_VERSION)
ERLANG_ROOT         := /usr

ERLANG_COMPILER     := $(ERLANG_ROOT)/bin/erlc

INC += -I.


ERLANG_COMPILER_OPT_DEBUG := 
ERLANG_COMPILER_OPT       := -b beam $(INC) -v -W2 $(ERLANG_COMPILER_OPT_DEBUG)


ERLANG_INTERPRETER_OPT_DEBUG := -init_debug

ERLANG_INTERPRETER  := $(ERLANG_ROOT)/bin/erl


FQDN    := `host \`hostname\` | awk '{ print $$1 }'`
VM_NAME := debug


COMMON = $(TOP)/common


# +v (verbose) option removed, as only active in the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
ERLANG_INTERPRETER_OPT := $(MOD) -name $(VM_NAME)@$(FQDN) \
	-pa $(COMMON) -run 


ERLANG_STARTUP_FUNCTION := run



