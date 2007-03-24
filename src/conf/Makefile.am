.PHONY: fix-version-file blank-MD5 clean-local real-clean

SUBDIRS = build doc environment

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = CeylanSettings.inc 
	


clean-local: 
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning build repository \
	@CEYLAN_DEFAULT_STYLE@"
	

real-clean: clean-local
	@echo "@CEYLAN_CLEAN_STYLE@    Deep cleaning of build repository \
	@CEYLAN_DEFAULT_STYLE@"
	-@rm -f Makefile.in config.guess config.sub install-sh ltmain.sh \
	missing stamp-h1


SHELL_SCRIPTS_SET = generateBugReport.sh

install-data-local:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Installing shell scripts in \
	$(srcdir) to [@CEYLAN_INSTALL_SHELL_DIR@]@CEYLAN_DEFAULT_STYLE@"
	@mkdir -p @CEYLAN_INSTALL_SHELL_DIR@
	@for f in $(SHELL_SCRIPTS_SET); do \
	cp -f $(srcdir)/$$f @CEYLAN_INSTALL_SHELL_DIR@/$$f; done
