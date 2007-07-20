PROJECT_NAME := Erlang
ARCHIVE_ROOT := $$HOME/Archive/Erlang
ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2


ERLANG_VERSION      := R9C-2
#ERLANG_ROOT        := /logiciels/public/otp_src_$(ERLANG_VERSION)
ERLANG_ROOT         := /usr

ERLANG_COMPILER     := $(ERLANG_ROOT)/bin/erlc


COMMON     = $(TOP)/common
WOOPER     = $(TOP)/wooper
WOOPER_SRC = $(WOOPER)/src

COMMON_INC = -I$(COMMON)
WOOPER_INC = -I$(WOOPER_SRC)

INC += -I. $(COMMON_INC) $(WOOPER_INC)

WARNING_LEVEL := 3

ERLANG_COMPILER_OPT_DEBUG := 
ERLANG_COMPILER_OPT       := -b beam $(INC) -v -W$(WARNING_LEVEL) \
	$(ERLANG_COMPILER_OPT_DEBUG)


ERLANG_INTERPRETER_OPT_DEBUG := -init_debug

ERLANG_INTERPRETER  := $(ERLANG_ROOT)/bin/erl


#FQDN    := `host \`hostname\` | awk '{ print $$1 }'`
#FQDN    := sinetix-ob

VM_NAME := debug

#DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN)
DISTRIBUTED_NODE_OPT :=


STARTUP_FUNCTION := run


# +v (verbose) option removed, as only active in the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
ERLANG_INTERPRETER_OPT := -pa $(COMMON) $(WOOPER_SRC) $(INC) \
	$(DISTRIBUTED_NODE_OPT)
