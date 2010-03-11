CEYLAN_ERLANG_TOP = .

MODULES_DIRS = common wooper traces tools


# No trace supervisor or graphical output wanted when running all tests from 
# a root directory (batch mode vs interactive one):	 	
CMD_LINE_OPT = "--batch"


PROJECT_NAME = "Ceylan-Erlang"



.PHONY: all test install clean



all:
	@echo "  Building $(PROJECT_NAME)"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done


test:
	@echo "  Testing $(PROJECT_NAME)"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s test CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done


install: all
	@echo "  Installing $(PROJECT_NAME) with INSTALLATION_PREFIX" \
	"set to $(INSTALLATION_PREFIX)"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s install CMD_LINE_OPT="${CMD_LINE_OPT}" \
	INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done


clean:
	@echo "  Cleaning $(PROJECT_NAME)"
	@for m in $(MODULES_DIRS); do \
	if ! ( if [ -d $$m ] ; then cd $$m && \
	$(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. \
	; else echo "     (directory $$m skipped)" ; fi ) ; \
	then exit 1; fi ; done

