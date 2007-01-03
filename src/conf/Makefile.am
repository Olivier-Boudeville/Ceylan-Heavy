.PHONY: fix-version-file blank-MD5 clean-local real-clean

SUBDIRS = build doc environment

DIST_SUBDIRS = $(SUBDIRS)

EXTRA_DIST = CeylanSettings.inc
	


clean-local: 
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning build repository\
	@CEYLAN_DEFAULT_STYLE@"
	

real-clean: clean-local
	@echo "@CEYLAN_CLEAN_STYLE@    Deep cleaning of build repository\
	@CEYLAN_DEFAULT_STYLE@"
	-@rm -f Makefile.in config.guess config.sub install-sh ltmain.sh \
	missing stamp-h1

