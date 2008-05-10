TOP = .


MODULES_DIRS = common wooper core



.PHONY: all tests bz2 clean 

all:

tests: all	

	
bz2: clean
	@mkdir -p $(ARCHIVE_ROOT)
	@cd ..; tar cvjf $(ARCHIVE_FILE) erlang
	@echo "    Archive for $(PROJECT_NAME) is available at: $(ARCHIVE_FILE)"
	
clean: clean-subdirs


# info target: see GNUmakerules.inc
			
include $(TOP)/GNUmakerules.inc

