CEYLAN_ERLANG_TOP = .


MODULES_DIRS = common wooper tools

WOOPER_TOP = $(CEYLAN_ERLANG_TOP)/wooper

include $(WOOPER_TOP)/GNUmakesettings.inc


# To be called from code/scripts Makefile.am, with INSTALL_TARGET defined:
install:
	@if [ ! -d "$(INSTALL_TARGET)" ]; then echo "Error, installation target '$(INSTALL_TARGET)' directory does not exist." ; fi
	@echo "  Installing Erlang developments to $(INSTALL_TARGET)"
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) -s install INSTALL_TARGET=$(INSTALL_TARGET) && cd .. ) ; then exit 1; fi ; done

