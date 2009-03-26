# Prerequisite: COMMON_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-common
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 0.1
endif


ARCHIVE_ROOT := $$HOME/Archive/$(PROJECT_NAME)

ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2


SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-snapshot.tar.bz2


PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2




# Source section.
COMMON_SRC := $(COMMON_TOP)/src

# BEAM path section.
COMMON_BEAM := $(COMMON_TOP)/src

# Include path section.
COMMON_INC = -I$(COMMON_SRC)


INC += -I. $(COMMON_INC)


# Erlang environment section.

ERLANG_ROOT     := ~/Software/Erlang-$(ERLANG_VERSION)

ERLANG_COMPILER := `PATH=$(ERLANG_ROOT)/bin:$$PATH which erlc`


WARNING_LEVEL := 3

ERLANG_COMPILER_OPT_DEBUG := +debug_info



ERLANG_INTERPRETER := `PATH=$(ERLANG_ROOT)/bin:$$PATH which erl`

ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp auto


ifndef FQDN

#FQDN    := `host \`hostname\` | awk '{ print $$1 }' | head -n 1`
FQDN    := `hostname -f`

endif


ifndef VM_TEST_NAME

VM_NAME := ceylan_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := ceylan_test

endif


#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(PROJECT_NAME)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(PROJECT_NAME)


STARTUP_FUNCTION := run

BEAM_DIRS += . $(COMMON_BEAM)


ERL_LAUNCHER = $(COMMON_SRC)/launch-erl.sh


# Add '+native' if native-code compilation is supported:
ERLANG_COMPILER_OPT := -b beam $(INC) -v -W$(WARNING_LEVEL)  \
	$(ERLANG_COMPILER_OPT_DEBUG)


# +v (verbose) option removed, as only active if the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)



# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options to the launcher afterwards:
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir $(dir))


ERL_PARAMETERIZED_LAUNCHER = $(ERL_LAUNCHER) -v -c $(PROJECT_NAME) --ln $(VM_TEST_NAME) --fqdn $(FQDN) $(BEAM_PATH_OPT)

