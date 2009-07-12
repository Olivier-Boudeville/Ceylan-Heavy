ERLANG_TRACES_TOP = .
	
# To be used from Ceylan's autotools-based build system:
TRACES_INSTALL_DIR = $(INSTALL_TARGET)/traces


# Tests could be removed:
install: all
	@echo "  Installing Erlang trace developments to $(TRACES_INSTALL_DIR)"
	@mkdir -p $(TRACES_INSTALL_DIR)
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) install INSTALL_TARGET=$(TRACES_INSTALL_DIR) && cd .. ) ; then exit 1; fi ; done


include $(ERLANG_TRACES_TOP)/GNUmakesettings.inc

