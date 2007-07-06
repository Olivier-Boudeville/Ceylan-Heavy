include $(TOP)/GNUmakevars.inc


.PHONY: launch test help clean clean-database info


SOURCES = $(wildcard *.erl)

BEAMS   = $(patsubst %.erl,%.beam,$(SOURCES))

TESTS   = $(wildcard *_test.erl)

TEST_TARGETS = $(patsubst %.erl,%,$(TESTS))



%.beam: %.erl
	@echo "    Compiling $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $< 


%: %.beam
	@echo "    Executing function $(ERLANG_STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) \
		$@ $(ERLANG_STARTUP_FUNCTION)

	
test: $(BEAMS)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t; done

	
launch:
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT)

	
	
			
help:
	@echo "To test hello.erl : 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"
	

clean: clean-database
	-@rm -f *.beam *.jam erl_crash.dump		


clean-database:
	-@rm -rf Mnesia.*@*


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
	
