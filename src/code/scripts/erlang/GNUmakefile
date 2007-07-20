TOP = .


# Add : simple client-server database directory launcher naming-service 
MODULES_DIRS = common wooper



.PHONY: all bz2 clean


all: 
	@echo "    Building all"
	@for d in $(MODULES_DIRS); do ( cd $$d; $(MAKE) all ); done
	
	
bz2: clean
	@mkdir -p $(ARCHIVE_ROOT)
	@cd ..; tar cvjf $(ARCHIVE_FILE) erlang
	@echo "    Archive for $(PROJECT_NAME) is available at : "\
		"$(ARCHIVE_FILE)"

		
include $(TOP)/GNUmakerules.inc
