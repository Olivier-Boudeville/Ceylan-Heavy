COMMON_TOP = .

MODULES_DIRS = src doc

include $(COMMON_TOP)/GNUmakesettings.inc


# To be used from Ceylan's autotools-based build system:
COMMON_INSTALL_DIR = $(INSTALL_TARGET)/common


# Tests could be removed:
install: all
	@echo "  Installing Erlang common developments to $(COMMON_INSTALL_DIR)"
	@mkdir -p $(COMMON_INSTALL_DIR)/ebin $(COMMON_INSTALL_DIR)/include $(COMMON_INSTALL_DIR)/src $(COMMON_INSTALL_DIR)/bin
	@/bin/cp src/*.beam $(COMMON_INSTALL_DIR)/ebin
	@/bin/cp src/*.hrl  $(COMMON_INSTALL_DIR)/include
	@/bin/cp src/*.erl  $(COMMON_INSTALL_DIR)/src
	@/bin/cp src/*.sh   $(COMMON_INSTALL_DIR)/bin
	
