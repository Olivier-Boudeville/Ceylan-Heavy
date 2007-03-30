SUBDIRS = generic interfaces logs maths middleware modules network system

# All programs using the Ceylan library should use in their 
# configure.ac the CEYLAN_PATH macro defined in ceylan.m4 :
EXTRA_DIST = \
	playTests.sh

# Disabled since this script is not to be run from an installed tree :
#bin_SCRIPTS = playTests.sh


check:
	@$(srcdir)/playTests.sh


install-data-local:
	@echo; echo " Installing playTests.sh"
	@mkdir -p $(DESTDIR)@CEYLAN_TEST_DIR@
	@cp -f $(srcdir)/playTests.sh $(DESTDIR)@CEYLAN_TEST_DIR@


clean-local:
	@echo " Cleaning all test logs"	
	-@/bin/rm -f  `find . -name 'testsOutcome.txt'`
	-@/bin/rm -f  `find . -name 'test*.log'`
	-@/bin/rm -f  `find . -name 'test*.xml'`
	-@/bin/rm -rf `find . -name 'testCeylan*-logs'`
