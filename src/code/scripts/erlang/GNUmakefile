TOP = .


MODULES_DIRS = common wooper

#MODULES_DIRS = simple client-server common database directory launcher naming-service 


.PHONY: all bz2 clean


all: 


bz2: clean
	@mkdir -p $(ARCHIVE_ROOT)
	@cd ..; tar cvjf $(ARCHIVE_FILE) erlang
	@echo "  Archive for $(PROJECT_NAME) is available at : "\
		"$(ARCHIVE_FILE)"

		
include $(TOP)/GNUmakerules.inc
