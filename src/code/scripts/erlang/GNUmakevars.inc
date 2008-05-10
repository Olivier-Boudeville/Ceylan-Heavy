
# Note: the order of definitions matters.


# Project section.

PROJECT_NAME := Ceylan-Erlang
ARCHIVE_ROOT := $$HOME/Archive/$(PROJECT_NAME)

ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2

SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-snapshot.tar.bz2



# PROJECT_NAME defined on a per-project basis.
ARCHIVE_ROOT := $$HOME/Archive/$(PROJECT_NAME)
ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2


PROJECT_VERSION := 0.1

PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2




# Ceylan section.

DOC_ROOT       = $(TOP)/../../../doc


# Ceylan WOOPER section.


WOOPER_VERSION := 0.2

WOOPER_RELEASE_BASE        := wooper-$(WOOPER_VERSION)

WOOPER_RELEASE_ARCHIVE_ZIP := $(WOOPER_RELEASE_BASE).zip
WOOPER_RELEASE_ARCHIVE_BZ2 := $(WOOPER_RELEASE_BASE).tar.bz2


COMMON     = $(TOP)/common
WOOPER     = $(TOP)/wooper
WOOPER_SRC = $(WOOPER)/src

WOOPER_DOC_DIR = $(DOC_ROOT)/web/main/documentation/wooper

COMMON_INC = -I$(COMMON)
WOOPER_INC = -I$(WOOPER_SRC)

INC += -I. $(COMMON_INC) $(WOOPER_INC)




# Erlang section.

ERLANG_ROOT         := /usr

ERLANG_COMPILER     := $(ERLANG_ROOT)/bin/erlc

ERLANG_COMPILER_OPT_DEBUG := 

# Add '+native' if native-code compilation is supported:
ERLANG_COMPILER_OPT       := -b beam $(INC) -v -W$(WARNING_LEVEL) \
	$(ERLANG_COMPILER_OPT_DEBUG)


ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp auto


WARNING_LEVEL := 3

ERLANG_COMPILER_OPT_DEBUG := 

# Add '+native' if native-code compilation is supported:
ERLANG_COMPILER_OPT       := -b beam $(INC) -v -W$(WARNING_LEVEL)  \
	$(ERLANG_COMPILER_OPT_DEBUG)


FQDN    := `host \`hostname\` | awk '{ print $$1 }' | head -n 1`
#FQDN    := rainbow.esperide.com
#FQDN    := `hostname -f`

VM_NAME := debug

VM_TEST_NAME := ceylan_test

#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(PROJECT_NAME)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(PROJECT_NAME)


STARTUP_FUNCTION := run

BEAM_DIRS = . $(COMMON) $(WOOPER_SRC)

BEAM_PATH_OPT = --beam-path $(BEAM_PATHS)

# +v (verbose) option removed, as only active in the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)

ERL_LAUNCHER=$(TOP)/common/launch-erl.sh

ERL_PARAMETRIZED_LAUNCHER=${ERL_LAUNCHER} -v -c $(PROJECT_NAME) --ln $(VM_TEST_NAME) --fqdn $(FQDN) $(BEAM_PATH_OPT)







# Dot (graphviz) section.

DOT_ROOT = /usr/bin

DOT_OPT = -Gcharset=latin1

DOT = $(DOT_ROOT)/dot $(DOT_OPT) 


# Gnuplot section.
GNUPLOT = gnuplot


# Image viewer section.
#IMG_VIEWER = /usr/bin/X11/xv
IMG_VIEWER = /usr/bin/eog

