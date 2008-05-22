include $(CEYLAN_ERLANG_TOP)/GNUmakevars.inc


.PHONY: all all-recurse launch test test-recurse build-tests                  \
	help-erl clean clean-erlang clean-database                            \
	clean-subdirs clean-dot clean-log clean-probe clean-gnuplot realclean \
	info info-files $(MODULES_DIRS)



SOURCES = $(wildcard *.erl)

# Includes test beams:
BEAMS   = $(patsubst %.erl,%.beam,$(SOURCES))

TESTS   = $(wildcard *_test.erl)

TEST_TARGETS = $(patsubst %.erl,%,$(TESTS))


# Outside prerequesites: $(WOOPER_SRC)/wooper.hrl wooper_class_manager


# Regarding dependency management.
#
# One can just ignore dependencies and rebuild "blindly" every module in 
# current subtree. One just has to think to issue 'make' in a right location
# before running the Erlang program.
# Another option is to track dependencies explicitly. We found no existing 
# way of managing dependencies automatically, thus they have to be declared by
# hand. We want these user-specified dependencies to work with the generic
# rules to build beam files and to run programs.
# For example, 'make class_MyObject_run' would check class_MyObject_test.beam
# and its dependencies before running it.
# A generic rule like: 'class_%.beam: class_%.erl' would not take into account
# dependencies.
# A generic rule like: 'class_%.beam: class_%.erl class_%_dependencies' with
# a definition like 'class_MyObject_dependencies: class_A.beam moduleB.beam'
# would work (and classes not depending on others could be managed with a 
# second generic rule: 'class_%.beam: class_%.erl'), but target like
# 'include $(TOP)/GNUmakerules.inc', not depending on their .PHONY status,
# are *always* rebuilt. In our case, class_MyObject.beam (and all its 
# prerequesites !) would thus always be rebuilt, even if no change at all was
# operated, with is unacceptable. 

# Thus we stick from now to the basic strategy: always issue a global 'make' 
# before running a program, otherwise be doomed (changes ignored at runtime).





all: all-recurse $(BEAMS)

all-recurse:
	@echo "   Building all in "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s all ); done


class_%.beam: class_%.erl class_%.hrl class_%_dependencies
	@echo "     Compiling class with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 

class_%.beam: class_%.erl class_%_dependencies
	@echo "     Compiling class $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 


class_%.beam: class_%.erl class_%.hrl
	@echo "     Compiling no-dependency class with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 

class_%.beam: class_%.erl
	@echo "     Compiling no-dependency class $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 




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
	@echo "     Running integration test $@ from $^ with $(ERL_PARAMETRIZED_LAUNCHER)"
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
	

# class_TimerManager_run -> class_TimeManager_test:run()
	
%_run: %_test %_test_dependencies
	@echo "     Running unitary test $@ (first form) from $^, with $(ERL_PARAMETRIZED_LAUNCHER)"
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)

%_run: %_test %.beam 
	@echo "     Running unitary test $@ (second form) from $^ with $(ERL_PARAMETRIZED_LAUNCHER)"
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
		
%_run: %_test
	@echo "     Running unitary test $@ (third form) from $^ with $(ERL_PARAMETRIZED_LAUNCHER)"
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
		
		
%_run: %_sim.beam %.beam %_run_dependencies
	@echo "     Running simulation $@ (two prerequesites) with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)

%_run: %_sim.beam %_run_dependencies
	@echo "     Running simulation $@ (one prerequesite) with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)
	
%_run: %_sim.beam %.beam
	@echo "     Running simulation $@ (two prerequesites)  with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)


test: all test-recurse
	@for t in $(TEST_TARGETS); do if ! $(MAKE) -s `echo $$t | sed 's|_test|_run|1'`; then return 1; fi ; done

test-recurse:
	@echo "   Testing all in "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s test ); done

# Not used anymore now that all beams are always built:	
build-tests: $(BEAMS)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t; done


launch: Emakefile
	@echo "     Launching interpreter with default Ceylan settings"
	@${ERL_PARAMETRIZED_LAUNCHER} --ln test_shell $(CMD_LINE_OPT)


# Allows for example 'make:files([class_BasicMeter]).' or 'make:all().':
Emakefile:
	@echo "     Generating Emakefile"
	@echo "{'*',[debug_info,{i,\"$(WOOPER_SRC)\"}]}." > Emakefile
		
		
help-erl:
	@echo "To test hello.erl: 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"


STATE_MACHINES = $(wildcard *.graph)

STATE_MACHINES_DIAGRAMS  = $(STATE_MACHINES:%.graph=%.png) 


# To trigger the automatic rendering of a created diagram, add 'VIEW_GRAPH=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_GRAPH=')
VIEW_GRAPH = no
#VIEW_GRAPH =

%.png: %.graph
	@echo "     Generating $@"
	@$(DOT) -o$@ -Tpng $<
	@if [ "$(VIEW_GRAPH)" != "no" ] ; then $(IMG_VIEWER) $@ & fi



# To trigger the automatic rendering of a created diagram, add 'VIEW_GRAPH=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_GRAPH=')
VIEW_PLOT = yes
#VIEW_PLOT =

%.png: %.p %.plotsim
	@echo "     Generating probe report $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ] ; then $(IMG_VIEWER) $@ & fi

%-generated.png: %.pdoc %.plotdoc
	@echo "     Generating plot report $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ] ; then $(IMG_VIEWER) $@ & fi


	
clean: clean-erlang clean-database clean-subdirs clean-dot clean-log \
	clean-probe clean-gnuplot


clean-erlang:
	@echo "     Cleaning all in "$(PWD) #`basename $(PWD)`
	-@rm -f *.beam *.jam erl_crash.dump


clean-database:
	-@rm -rf Mnesia.*@*


clean-subdirs:
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s clean ); done


# Some .png must be kept (ex: dia-exported ones).
clean-dot:
	-@rm -f $(STATE_MACHINES_DIAGRAMS) *.map	

clean-log:
	-@rm -f *.log

clean-probe:
	-@rm -f *_Probe*.png *_probe*.png *-generated.png
	
clean-gnuplot:
	@# Let's hope we do not remove user files:
	-@rm -f *.p *.plotsim
	
	
realclean: clean
	@echo "     Deep cleaning in "`basename $(PWD)`
	@-rm -f Emakefile Ceylan-*.tar.bz2 Ceylan-*.zip 


info: info-files
	@echo "FQDN                   = $(FQDN)"
	@echo "ARCHIVE_FILE           = $(ARCHIVE_FILE)"
	@echo "ERLANG_INTERPRETER     = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_INTERPRETER_OPT = $(ERLANG_INTERPRETER_OPT)"
	@echo "ERLANG_SRC             = $(ERLANG_SRC)"


info-files:	
	@echo "SOURCES      = $(SOURCES)"
	@echo "BEAMS        = $(BEAMS)"
	@echo "TESTS        = $(TESTS)"
	@echo "TEST_TARGETS = $(TEST_TARGETS)"
	
