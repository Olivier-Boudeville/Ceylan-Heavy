
# Note: the order of definitions matters.

CEYLAN_TRUNK = $(CEYLAN_ERLANG_TOP)/../../../..


# Project section.

# PROJECT_NAME and PROJECT_VERSION should be already defined, on a per-project
# basis.


ARCHIVE_ROOT := $$HOME/Archive/$(PROJECT_NAME)

ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2

SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-snapshot.tar.bz2


PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2


ERLANG_VERSION      := R9C-2
ERLANG_ROOT         := /usr

ERLANG_COMPILER     := $(ERLANG_ROOT)/bin/erlc


# Directory section.

FRAMEWORK_DIR = $(CEYLAN_ERLANG_TOP)


COMMON_BASE_DIR  := $(FRAMEWORK_DIR)/common
WOOPER_BASE_DIR  := $(FRAMEWORK_DIR)/wooper
CORE_BASE_DIR    := $(FRAMEWORK_DIR)/core

GENERIC_BASE_DIR := $(CORE_BASE_DIR)/generic
MESH_BASE_DIR    := $(CORE_BASE_DIR)/mesh
SPACE_BASE_DIR   := $(CORE_BASE_DIR)/space


# Source section.

COMMON_SRC := $(COMMON_BASE_DIR)/src
WOOPER_SRC := $(WOOPER_BASE_DIR)/src
CORE_SRC   := $(CORE_BASE_DIR)/src

GENERIC_SRC := $(GENERIC_BASE_DIR)/src
MESH_SRC    := $(MESH_BASE_DIR)/src
SPACE_SRC   := $(SPACE_BASE_DIR)/src


# BEAM path section.

COMMON_BEAM := $(COMMON_BASE_DIR)/src
WOOPER_BEAM := $(WOOPER_BASE_DIR)/src
CORE_BEAM   := $(CORE_BASE_DIR)/src

GENERIC_BEAM := $(GENERIC_BASE_DIR)/src
MESH_BEAM    := $(MESH_BASE_DIR)/src
SPACE_BEAM   := $(SPACE_BASE_DIR)/src


# Include path section.

COMMON_INC  = -I$(COMMON_SRC)
WOOPER_INC  = -I$(WOOPER_SRC)
CORE_INC    = -I$(CORE_SRC)

GENERIC_INC = -I$(GENERIC_SRC)
MESH_INC    = -I$(MESH_SRC)
SPACE_INC   = -I$(SPACE_SRC)



INC += -I. $(COMMON_INC) $(WOOPER_INC) $(CORE_INC)                     \
	$(GENERIC_INC) $(MESH_INC) $(SPACE_INC)                            \


ERLANG_INTERPRETER  := $(ERLANG_ROOT)/bin/erl

ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp auto


WARNING_LEVEL := 3

ERLANG_COMPILER_OPT_DEBUG := 

# Add '+native' if native-code compilation is supported:
ERLANG_COMPILER_OPT       := -b beam $(INC) -v -W$(WARNING_LEVEL)  \
	$(ERLANG_COMPILER_OPT_DEBUG)



DOT_ROOT = /usr/bin

DOT_OPT = -Gcharset=latin1

DOT = $(DOT_ROOT)/dot $(DOT_OPT) 


GNUPLOT = gnuplot

#RST_GENERATOR = update-docutils.sh
RST_GENERATOR = generate-docutils.sh

#IMG_VIEWER = /usr/bin/X11/xv
IMG_VIEWER = /usr/bin/eog

PDF_VIEWER = /usr/bin/evince

CEYLAN_CSS = $(CEYLAN_TRUNK)/src/doc/web/common/css/Ceylan-docutils.css


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

# Used to use 'BEAM_DIRS := ... $(BEAM_DIRS)' but it led to multiple duplicates
# (multiple inclusions ?)
BEAM_DIRS = . $(COMMON_BEAM) $(WOOPER_BEAM) $(CORE_BEAM)                   \
	$(GENERIC_BEAM) $(MESH_BEAM) $(SPACE_BEAM)                             \


# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options afterwards:
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir $(dir))


# +v (verbose) option removed, as only active in the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)

ERL_LAUNCHER = $(COMMON_SRC)/launch-erl.sh

ERL_PARAMETRIZED_LAUNCHER = $(ERL_LAUNCHER) -v -c $(PROJECT_NAME) --ln $(VM_TEST_NAME) --fqdn $(FQDN) $(BEAM_PATH_OPT)

OVERALL_DOCUMENT_SOURCE = Ceylan-Erlang-manual.rst
OVERALL_DOCUMENT_TARGET = Ceylan-Erlang-manual.pdf

TMP_RST_REPOSITORY = tmp-rst

