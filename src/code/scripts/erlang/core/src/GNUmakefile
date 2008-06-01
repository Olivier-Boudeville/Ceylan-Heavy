CEYLAN_ERLANG_TOP = ../..

# Avoid declaring *_test targets as PHONY, otherwise they will not be executed
# when appropriate.
.PHONY: all elements simulation-launch tests tests-local test-TimeManager \
	test-RandomManager clean-dat shell help 
	


MODULES_DIRS =
	


include $(CEYLAN_ERLANG_TOP)/GNUmakerules.inc


clean: clean-dat


clean-dat:
	-@rm -f *.dat
	
	
Test_probe.dat: 
	@$(MAKE) class_Probe_run


shell:
	@$(ERL_PARAMETRIZED_LAUNCHER)
	 

