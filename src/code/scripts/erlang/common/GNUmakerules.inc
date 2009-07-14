
# From here all variables are expected to be updated, as first all 
# the variables of englobing projects should be set, then only rules
# based on that variables should be defined.



# Now defining the generic rules operating on the variables.

ERL_FILES = $(wildcard *.erl)

# Includes test beams:
BEAM_FILES   = $(patsubst %.erl,%.beam,$(ERL_FILES))

TEST_SOURCES = $(wildcard *_test.erl)

TEST_TARGETS = $(patsubst %.erl,%,$(TEST_SOURCES))


# Regarding dependency management.
#
# One can just ignore dependencies and rebuild "blindly" every module in 
# current subtree.
# One just has not to forget to issue 'make' from a right location, before
# running one's Erlang program.

# Another option is to track dependencies explicitly.
# We found no existing way of managing dependencies automatically, thus 
# they have to be declared by hand.
# We want these user-specified dependencies to work with the generic
# rules to build beam files and to run programs.
# For example, 'make MyObject_run' would check MyObject_test.beam
# and its dependencies before running it.
# A generic rule like: '%.beam: %.erl' would not take into account
# dependencies.
# A generic rule like: '%.beam: %.erl %_dependencies' with
# a definition like 'MyObject_dependencies: A.beam moduleB.beam'
# would work (and modules not depending on others could be managed with a 
# second generic rule: '%.beam: %.erl'), but generic targets, as 
# defined in GNUmakerules.inc, seem to be *always* rebuilt, not depending
# on their .PHONY status.
# In our case, MyObject.beam (and all its 
# prerequesites!) would thus always be rebuilt, even if no change at all was
# operated on the corresponding sources, which whould not be acceptable. 

# Thus we stick from now to the basic strategy: always issue a global 'make'
# at the root of the source before running a program, otherwise be doomed
# (changes not taken into account at runtime, as not recompiled).



.PHONY: all all-recurse test test-recurse doc-recurse build-tests launch \
  install-all install-announce install-ebin install-bin install-src \
  install-include install-examples install-test install-doc \
  help-erl clean clean-erlang clean-database clean-recurse realclean \
  info info-files $(MODULES_DIRS)



all: all-recurse $(BEAM_FILES)


all-recurse:
	@echo "   Building all in "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done
	
	
%.beam: %.erl %.hrl
	@echo "     Compiling module with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 


%.beam: %.erl
	@echo "     Compiling module $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 



#%_test: %.beam %_test.beam
#	@echo "    Executing test function $(STARTUP_FUNCTION) in module $@"
#	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)

#%_test: %_test.beam
#	@echo "    Executing test function $(STARTUP_FUNCTION) in module $@"
#	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)

%_test: %.beam %_test.beam

%_test: %_test.beam

	
%_interactive_test: %.beam %_interactive_test.beam
	@echo "     Executing interactively test function $(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)


%_batch_test: %.beam %_batch_test.beam
	@echo "     Executing non-interactively test function $(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT)	-run $@ $(STARTUP_FUNCTION)



# _integration prefix added not to match instead of the next rule.
# %_integration_dependencies target is a phony target, so that test dependencies
# can be specified.
%_integration_run: %_integration_test.beam %_integration_dependencies
	@echo "     Running integration test $@ from $^ with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
	


# 'X_run' becomes 'X_test:run()':
	
%_run: %_test %_test_dependencies
	@echo "     Running unitary test $@ (first form) from $^, with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)


%_run: %_test %.beam 
	@echo "     Running unitary test $@ (second form) from $^"
	$(ERL_PARAMETERIZED_LAUNCHER) --ln $@  --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
	
		
%_run: %_test
	@echo "     Running unitary test $@ (third form) from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)


test: all test-recurse
	@for t in $(TEST_TARGETS); do if ! $(MAKE) -s `echo $$t | sed 's|_test|_run|1'` CMD_LINE_OPT="${CMD_LINE_OPT}"; then exit 1; fi ; done


test-recurse:
	@echo "   Testing all in "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s test CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done


# Best placed here rather than in GNUmakerules-docutils.inc:
doc: doc-recurse

doc-recurse:
	@echo "   Preparing documentation in "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s doc CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# Not used any more now that all beams are always built:	
build-tests: $(BEAM_FILES)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t CMD_LINE_OPT="${CMD_LINE_OPT}"; done


launch: Emakefile
	@echo "     Launching interpreter with default Ceylan settings"
	@${ERL_PARAMETERIZED_LAUNCHER} --ln test_shell $(CMD_LINE_OPT)
	


# Installation section.


install: all doc install-all


install-all: install-announce install-bin install-ebin install-src \
	install-include install-examples install-test install-doc
	
	
install-announce:
	@echo "  Installing package $(PACKAGE_NAME) in $(PACKAGE_INSTALLATION_PREFIX)"
	
	
install-bin:
	@if [ -n "$(EXEC_TO_INSTALL)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/bin ; \
	cp -r $(EXEC_TO_INSTALL) $(PACKAGE_INSTALLATION_PREFIX)/bin ; fi


install-ebin:
	@if [ -n "$(BEAMS_TO_INSTALL)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/ebin ; \
	cp -r $(BEAMS_TO_INSTALL) $(PACKAGE_INSTALLATION_PREFIX)/ebin ; fi

 
install-src:
	@if [ -n "$(SOURCES_TO_INSTALL)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/src ; \
	cp -r $(SOURCES_TO_INSTALL) $(PACKAGE_INSTALLATION_PREFIX)/src ; fi


install-include:
	@if [ -n "$(INCLUDES_TO_INSTALL)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/include ; \
	cp -r $(INCLUDES_TO_INSTALL) $(PACKAGE_INSTALLATION_PREFIX)/include ; fi


install-examples:
	@if [ -n "$(EXAMPLES_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/examples/ebin ; \
	cp -r $(EXAMPLES_TO_INSTALL_BEAMS) \
	$(PACKAGE_INSTALLATION_PREFIX)/examples/ebin ; fi
	@if [ -n "$(EXAMPLES_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/examples/src ; \
	cp -r $(EXAMPLES_TO_INSTALL_SRC) \
	$(PACKAGE_INSTALLATION_PREFIX)/examples/src ; fi


install-test:
	@if [ -n "$(TESTS_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/test/ebin ; \
	cp -r $(TESTS_TO_INSTALL_BEAMS) \
	$(PACKAGE_INSTALLATION_PREFIX)/test/ebin ; fi
	@if [ -n "$(TESTS_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/test/src ; \
	cp -r $(TESTS_TO_INSTALL_SRC) \
	$(PACKAGE_INSTALLATION_PREFIX)/test/src ; fi


install-doc:
	@if [ -n "$(DOC_TO_INSTALL)" ] ; then \
	mkdir -p $(PACKAGE_INSTALLATION_PREFIX)/doc ; \
	cp -r $(DOC_TO_INSTALL) $(PACKAGE_INSTALLATION_PREFIX)/doc ; fi

	
	
help-erl:
	@echo "To test hello.erl: 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"

clean: clean-erlang clean-database clean-recurse


clean-erlang:
	@echo "     Cleaning all in "$(PWD) #`basename $(PWD)`
	-@rm -f *.beam *.jam erl_crash.dump


clean-database:
	-@rm -rf Mnesia.*@*
	
	
clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done

	
realclean: clean
	@echo "     Deep cleaning in "`basename $(PWD)`
	@-rm -f Emakefile
	
	
info: info-files
	@echo "FQDN                   = $(FQDN)"
	@echo "BEAM_DIRS              = $(BEAM_DIRS)"
	@echo "BEAM_PATH_OPT          = $(BEAM_PATH_OPT)"
	@echo "ARCHIVE_FILE           = $(ARCHIVE_FILE)"
	@echo "ERLANG_INTERPRETER     = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_INTERPRETER_OPT = $(ERLANG_INTERPRETER_OPT)"
	@echo "ERLANG_SRC             = $(ERLANG_SRC)"
	@echo "VM_TEST_NAME           = $(VM_TEST_NAME)"
	@echo "PROJECT_NAME           = $(PROJECT_NAME)"
	
	
info-files:	
	@echo "ERL_FILES    = $(ERL_FILES)"
	@echo "BEAM_FILES   = $(BEAM_FILES)"
	@echo "TEST_SOURCES = $(TEST_SOURCES)"
	@echo "TEST_TARGETS = $(TEST_TARGETS)"
	
	
info-install:
	@echo "BEAMS_TO_INSTALL = $(BEAMS_TO_INSTALL)"
	
