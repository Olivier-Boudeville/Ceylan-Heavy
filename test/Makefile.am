SUBDIRS = generic interfaces logs maths modules network system

# All programs using the Ceylan library should use in their 
# configure.ac the CEYLAN_PATH macro defined in ceylan.m4 :
EXTRA_DIST = ceylan.m4 playTests.sh

# Disabled since this script is not to be run from an installed tree :
#bin_SCRIPTS = playTests.sh
data_DATA = ceylan.m4


check:
	@./playTests.sh


clean-local:
	@echo " Cleaning all test logs"	
	-@/bin/rm -f `find . -name 'testsOutcome.txt'`
	-@/bin/rm -f `find . -name 'test*.log'`
	-@/bin/rm -rf `find . -name 'testCeylan*-logs'`
