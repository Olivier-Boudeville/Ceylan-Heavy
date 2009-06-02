ERLANG_TOOLS_TOP = .


MODULES_DIRS = resource-indexer


	
# To be used from Ceylan's autotools-based build system:
TOOLS_INSTALL_DIR = $(INSTALL_TARGET)/tools


# Tests could be removed:
install: all
	@echo "  Installing Erlang tools developments to $(TOOLS_INSTALL_DIR)"
	@mkdir -p $(TOOLS_INSTALL_DIR)
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) install INSTALL_TARGET=$(TOOLS_INSTALL_DIR) && cd .. ) ; then exit 1; fi ; done


include $(ERLANG_TOOLS_TOP)/GNUmakesettings.inc

