CEYLAN_ERLANG_TOP = ..


.PHONY: all release-wooper release-wooper-zip release-wooper-bz2   \
	prepare-wooper clean-wooper-release clean-wooper-archive

 
MODULES_DIRS = src doc examples 


all:

release-wooper: release-wooper-zip release-wooper-bz2


release-wooper-zip: prepare-wooper
	@echo "  Creating Wooper release archive $(WOOPER_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(WOOPER_RELEASE_ARCHIVE_ZIP) $(WOOPER_RELEASE_BASE) 
	
release-wooper-bz2: prepare-wooper
	@echo "  Creating Wooper release archive $(WOOPER_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar cvjf $(WOOPER_RELEASE_ARCHIVE_BZ2) $(WOOPER_RELEASE_BASE)
	
	
prepare-wooper: clean clean-wooper-release
	@echo "  Preparing release archive for Wooper $(WOOPER_VERSION)"
	@cd .. && mkdir -p $(WOOPER_RELEASE_BASE) && /bin/cp -r common wooper GNUmake* $(WOOPER_RELEASE_BASE)
	@-cd.. && find $(WOOPER_RELEASE_BASE) -type d -a -name '.svn' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	@cd .. && /bin/cp -f $(WOOPER_DOC_DIR)/index.html \
		$(WOOPER_RELEASE_BASE)/wooper/doc/wooper.html

clean: clean-wooper-archive
		
clean-wooper-release:
	@echo "  Cleaning release archive for Wooper"
	-@cd .. && /bin/rm -rf $(WOOPER_RELEASE_BASE)
	
clean-wooper-archive:
	-@/bin/rm -f wooper-*.tar.bz2 wooper-*.zip
	
	
include $(CEYLAN_ERLANG_TOP)/GNUmakerules.inc

