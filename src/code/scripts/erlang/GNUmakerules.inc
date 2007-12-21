include $(TOP)/GNUmakevars.inc


.PHONY: launch test help clean clean-database info


SOURCES = $(wildcard *.erl)

BEAMS   = $(patsubst %.erl,%.beam,$(SOURCES))

TESTS   = $(wildcard *_test.erl)

TEST_TARGETS = $(patsubst %.erl,%,$(TESTS))


class_%.beam: class_%.erl $(WOOPER_SRC)/wooper.hrl
	@echo "    Compiling class $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 


%.beam: %.erl
	@echo "    Compiling module $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 


%_test: %.beam %_test.beam
	@echo "    Executing test function $(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)

%_test: %_test.beam
	@echo "    Executing test function $(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)


%:%.beam

	
test: $(BEAMS)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t; done

	
launch:
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT)

			
help:
	@echo "To test hello.erl : 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"

	
clean: clean-database clean-subdirs
	@echo "    Cleaning all in "`basename $(PWD)`
	-@rm -f *.beam *.jam erl_crash.dump		

clean-database:
	-@rm -rf Mnesia.*@*

clean-subdirs:
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s clean ); done


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
	
