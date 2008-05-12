TOP = .


MODULES_DIRS = common wooper core

# Defined here to avoid polluting other projects using the Ceylan ruleset:
PROJECT_NAME := Ceylan-Erlang

PROJECT_VERSION := 0.1


.PHONY: all tests bz2 clean 

# Must be defined explicitly as GNUmakerules.inc not included here:
all:
	@echo "   Building all from "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s all ); done


# Special test target with disabled trace supervisor when run from source root:
test: all
	@echo "   Testing all from "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s test CMD_LINE_OPT="-no-supervisor" ); done

	
bz2: clean
	@mkdir -p $(ARCHIVE_ROOT)
	@cd ..; tar cvjf $(ARCHIVE_FILE) erlang
	@echo "    Archive for $(PROJECT_NAME) is available at: $(ARCHIVE_FILE)"
	
	
clean:
	@echo "   Cleaning all from "`basename $(PWD)`
	@for m in $(MODULES_DIRS); do ( cd $$m; $(MAKE) -s clean ); done


# info target: see GNUmakerules.inc
			
# Commented to define a special test target: include $(TOP)/GNUmakerules.inc
# Instead (for PROJECT_NAME for bz2:)
include $(TOP)/GNUmakevars.inc

