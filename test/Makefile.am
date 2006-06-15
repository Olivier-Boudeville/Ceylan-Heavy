SUBDIRS = generic interfaces logs maths modules system

# All programs using the Ceylan library should use in their 
# configure.ac the CEYLAN_PATH macro defined in ceylan.m4 :
EXTRA_DIST = ceylan.m4 playTests.sh

check:
	@./playTests.sh


clean-local:
	@echo " Cleaning all test logs"	
	-@/bin/rm -f `find . -name 'test*.log'`
	-@/bin/rm -rf `find . -name 'testCeylan*-logs'`
