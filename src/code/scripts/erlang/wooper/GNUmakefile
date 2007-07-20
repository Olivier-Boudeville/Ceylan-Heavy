TOP = ..


.PHONY: all
 
MODULES_DIRS = src examples 


all:
	@for d in $(MODULES_DIRS); do ( cd $$d; $(MAKE) all ); done

	
include $(TOP)/GNUmakerules.inc

