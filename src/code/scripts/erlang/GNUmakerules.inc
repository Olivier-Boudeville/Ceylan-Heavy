include $(CEYLAN_ERLANG_TOP)/GNUmakevars.inc


.PHONY: all all-recurse launch test test-recurse build-tests              \
	doc doc-recurse help-erl                                              \
	clean clean-erlang clean-database                                     \
	clean-subdirs clean-dot clean-log clean-probe clean-gnuplot realclean \
	info info-files $(MODULES_DIRS)



ERL_FILES = $(wildcard *.erl)

# Includes test beams:
BEAM_FILES   = $(patsubst %.erl,%.beam,$(ERL_FILES))

TEST_SOURCES = $(wildcard *_test.erl)

TEST_TARGETS = $(patsubst %.erl,%,$(TEST_SOURCES))


RST_FILES = $(wildcard *.rst)

AUX_GENERATED_FILES  = $(patsubst %.rst,%.aux,$(RST_FILES))
TEX_GENERATED_FILES  = $(patsubst %.rst,%.tex,$(RST_FILES))
OUT_GENERATED_FILES  = $(patsubst %.rst,%.out,$(RST_FILES))
HTML_GENERATED_FILES = $(patsubst %.rst,%.html,$(RST_FILES))
PDF_GENERATED_FILES  = $(patsubst %.rst,%.pdf,$(RST_FILES))

RST_GENERATED_FILES =  $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)      \
	$(OUT_GENERATED_FILES) $(HTML_GENERATED_FILES) $(PDF_GENERATED_FILES)   


PNG_FROM_PYTHON = $(patsubst %.py,%.png,$(wildcard *.py))
PNG_FROM_PYTHON_NEGATED = $(patsubst %.py,%-negated.png,$(wildcard *.py))

NEGATED_PNG = $(patsubst %.png,%-negated.png,$(wildcard *.png))

GRAPH_SRC = $(wildcard *.graph)

GRAPH_PNG  = $(GRAPH_SRC:%.graph=%.png) 


# OVERALL_DOCUMENT_SOURCE defined in per-project GNUmakefile:
OVERALL_DOCUMENT_TARGET = $(patsubst %.rst,%.pdf,$(OVERALL_DOCUMENT_SOURCE))



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





all: all-recurse $(BEAM_FILES)

all-recurse:
	@echo "   Building all in "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ) ; then return 1; fi ; done


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
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
	

# class_TimerManager_run -> class_TimeManager_test:run()
	
%_run: %_test %_test_dependencies
	@echo "     Running unitary test $@ (first form) from $^, with $(ERL_PARAMETRIZED_LAUNCHER)"
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)

%_run: %_test %.beam 
	@echo "     Running unitary test $@ (second form) from $^"
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@  --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
		
%_run: %_test
	@echo "     Running unitary test $@ (third form) from $^"
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_test:run()|1'` $(CMD_LINE_OPT)
		
		
%_run: %_sim.beam %.beam %_run_dependencies
	@echo "     Running simulation $@ (two prerequesites) with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)

%_run: %_sim.beam %_run_dependencies
	@echo "     Running simulation $@ (one prerequesite) with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)
	
%_run: %_sim.beam %.beam
	@echo "     Running simulation $@ (two prerequesites)  with $(ERL_PARAMETRIZED_LAUNCHER)" 
	@$(ERL_PARAMETRIZED_LAUNCHER) --ln $@ --eval `echo $@ | sed 's|_run|_sim:run()|1'` $(CMD_LINE_OPT)


test: all test-recurse
	@for t in $(TEST_TARGETS); do if ! $(MAKE) -s `echo $$t | sed 's|_test|_run|1'` CMD_LINE_OPT="${CMD_LINE_OPT}"; then return 1; fi ; done


test-recurse:
	@echo "   Testing all in "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) -s test CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ) ; then return 1; fi ; done


# Not used anymore now that all beams are always built:	
build-tests: $(BEAM_FILES)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t CMD_LINE_OPT="${CMD_LINE_OPT}"; done


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



# To trigger the automatic rendering of a created diagram, add 'VIEW_PLOT=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_PLOT=')
#VIEW_PLOT = yes
VIEW_PLOT = no

%.png: %.plot %.dat
	@echo "     Generating probe report $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ] ; then $(IMG_VIEWER) $@ & fi


%-negated.png: %.png
	@echo "    Generating $@ from $< with $(IMAGE_MAGICK)"
	@$(IMAGE_MAGICK) $< -negate $@


%.dat: %.py
	@echo "     Generating plot data $@ from $<"
	@$(PYTHON) $<


# To trigger the automatic rendering of a created PDF, add 'VIEW_PDF=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_PDF=')
VIEW_PDF = yes
#VIEW_PDF = no


%.pdf: %.rst
	@echo "     Generating PDF documentation $@"
	@if $(RST_GENERATOR) $< --pdf ; then if [ "$(VIEW_PDF)" != "no" ] ; then $(PDF_VIEWER) $@ & fi; fi
	
%.html: %.rst
	@echo "     Generating HTML documentation $@"
	$(RST_GENERATOR) $< $(CEYLAN_CSS)
	



doc: $(GRAPH_PNG) $(PNG_FROM_PYTHON) $(PNG_FROM_PYTHON_NEGATED) doc-recurse #$(PDF_GENERATED_FILES)


doc-recurse:
	@echo "   Preparing documentation in "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) -s doc CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ) ; then return 1; fi ; done




# To trigger the automatic rendering of a created diagram, add 'VIEW_GRAPH=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_GRAPH=')
#VIEW_GRAPH = yes
VIEW_GRAPH = no


%.png: %.graph
	@echo "    Generating $@"
	@$(DOT) -o$@ -Tpng $<
	@if [ "$(VIEW_GRAPH)" != "no" ] ; then $(IMG_VIEWER) $@ & fi



	
clean: clean-erlang clean-database clean-dot clean-log \
	clean-probe clean-gnuplot clean-png clean-rst clean-recurse


clean-erlang:
	@echo "     Cleaning all in "$(PWD) #`basename $(PWD)`
	-@rm -f *.beam *.jam erl_crash.dump


clean-database:
	-@rm -rf Mnesia.*@*


# Some .png must be kept (ex: dia-exported ones).
clean-dot:
	-@rm -f $(STATE_MACHINES_DIAGRAMS) *.map	


clean-log:
	-@rm -f *.log


clean-probe:
	-@if [ `basename $$PWD` != "doc" ]; then rm -f *_Probe*.png *_probe*.png *-generated.png; fi


clean-gnuplot:
	@# Let's hope we do not remove user files:
	-@if [ `basename $$PWD` != "doc" ]; then rm -f *.dat; fi

	
clean-png:
	-@rm -f $(GRAPH_PNG) $(PNG_FROM_PYTHON) $(PNG_FROM_PYTHON_NEGATED) $(NEGATED_PNG)
	
	
clean-rst:
	-@rm -f $(RST_GENERATED_FILES)	


clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( cd $$m && $(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ) ; then return 1; fi ; done

	
realclean: clean
	@echo "     Deep cleaning in "`basename $(PWD)`
	@-rm -f Emakefile Ceylan-*.tar.bz2 Ceylan-*.zip 


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
	
