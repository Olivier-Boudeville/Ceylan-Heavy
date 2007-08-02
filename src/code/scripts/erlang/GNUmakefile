TOP = .


MODULES_DIRS = common wooper



.PHONY: all bz2 clean release-wooper release-wooper-zip release-wooper-bz2 \
	prepare-wooper clean-wooper-release clean-archive


all: 
	@echo "   Building all"
	@for d in $(MODULES_DIRS); do ( cd $$d; $(MAKE) all ); done
	
	
bz2: clean
	@mkdir -p $(ARCHIVE_ROOT)
	@cd ..; tar cvjf $(ARCHIVE_FILE) wooper-$(WOOPER_VERSION)
	@echo "    Archive for $(PROJECT_NAME) is available at: "\
		"$(ARCHIVE_FILE)"

release-wooper: release-wooper-zip release-wooper-bz2


release-wooper-zip: prepare-wooper
	@echo "  Creating Wooper release archive $(WOOPER_RELEASE_ARCHIVE_ZIP)"
	@zip -r $(WOOPER_RELEASE_ARCHIVE_ZIP) $(WOOPER_RELEASE_BASE) 
	
release-wooper-bz2: prepare-wooper
	@echo "  Creating Wooper release archive $(WOOPER_RELEASE_ARCHIVE_BZ2)"
	@tar cvjf $(WOOPER_RELEASE_ARCHIVE_BZ2) $(WOOPER_RELEASE_BASE)
	
	
prepare-wooper: clean clean-wooper-release
	@echo "  Preparing release archive for Wooper $(WOOPER_VERSION)"
	@mkdir -p $(WOOPER_RELEASE_BASE)
	@/bin/cp -r common wooper GNUmake* $(WOOPER_RELEASE_BASE)
	@-find $(WOOPER_RELEASE_BASE) -type d -a -name '.svn' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	@/bin/cp -f $(WOOPER_DOC_DIR)/index.html \
		$(WOOPER_RELEASE_BASE)/wooper/doc/wooper.html
		
clean-wooper-release:
	@echo "  Cleaning release archive for Wooper"
	-@/bin/rm -rf $(WOOPER_RELEASE_BASE)
	
	
clean: clean-archive

clean-archive:
	-@/bin/rm -f wooper-*.tar.bz2 wooper-*.zip
	
			
include $(TOP)/GNUmakerules.inc
