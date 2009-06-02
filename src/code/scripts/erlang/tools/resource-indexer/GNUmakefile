ERLANG_TOOLS_TOP = ..


include $(ERLANG_TOOLS_TOP)/GNUmakesettings.inc


# To be used from Ceylan's autotools-based build system:
INDEXER_INSTALL_DIR = $(INSTALL_TARGET)/resource-indexer


# Tests could be removed:
install: all
	@echo "  Installing Erlang resource indexer to $(INDEXER_INSTALL_DIR)"
	@mkdir -p $(INDEXER_INSTALL_DIR)
	@/bin/cp *.sh *.beam $(INDEXER_INSTALL_DIR)

