

CEYLAN_ERLANG_TOP = .

MODULES_DIRS = common wooper traces tools


# No trace supervisor or graphical output wanted when running all tests from 
# a root directory (batch mode vs interactive one):	 	
CMD_LINE_OPT = "--batch"


PROJECT_NAME = "Ceylan-Erlang"


install:
	@echo "  Installing $(PROJECT_NAME) in default locations"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s install CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done

clean:
	@echo "  Cleaning $(PROJECT_NAME)"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done

