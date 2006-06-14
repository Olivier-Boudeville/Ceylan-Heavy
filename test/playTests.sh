#!/bin/bash

TESTLOGFILE=`pwd`"/testsOutcome.txt"

USAGE="`basename $0` [--interactive] : executes all tests for Ceylan in a row.\n\tIf the --interactive option is used, tests will not be run in batch mode, and will prompt the user for various inputs. Otherwise only their final result will be output. In all cases their messages will be stored in file ${TESTLOGFILE}. The return code of this script will be the number of failed tests (beware to overflow of the return code)"
 
 
# In batch (non-interactive) mode by default (0) :
is_batch=0



if [ "$#" -ge "2" ] ; then
	echo -e "Usage : $USAGE" 1>&2
	exit 1
fi	

if [ "$#" == "1" ] ; then
	if [ "$1" != "--interactive" ] ; then
		echo "$1 : unknown option." 1>&2
		echo -e "Usage : $USAGE" 1>&2
		exit 2
	else
		is_batch=1	
	fi
fi



# Debug mode is deactivated by default (1).
debug_mode=1


DEBUG_INTERNAL()
# Prints debug informations if debug mode is on.
{
	[ "$debug_mode" == 1 ] || echo "Debug : $*"
}


WARNING_INTERNAL()
# Prints warning informations to error output.
{
	echo "Warning : $*" 1>&2
}


ERROR_INTERNAL()
# Prints error informations to error output.
{
	echo "Error : $*" 1>&2
}


DEBUG_INTERNAL "Debug mode activated"

# Try to find term utilities :

TEST_ROOT=`dirname $0`

TERM_PATH="$TEST_ROOT/../src/code/scripts/shell/termUtils.sh"
if [ -f "$TERM_PATH" ] ; then
	. $TERM_PATH
else
	ERROR_INTERNAL "terminal utilities not found (was expecting : $TERM_PATH)"
	exit
fi	


# Creates a test directory to avoid polluting other directories :
TEST_DIR="tests-results-"`date '+%Y%m%d'`


if [ "$is_batch" == "0" ] ; then
	echo -e "\nRunning in batch mode, tests will be silent, only results are to be output."
else	
	echo -e "\nInteractive tests will only need the enter key to be pressed one or more times."
fi

echo -e "(be warned that some tests might take a long time, and that some of them have no special output except a test result)"


# This script will automatically run each test of each selected Ceylan module.
#TESTED_MODULES="generic logs interfaces modules system maths"
TESTED_ROOT_MODULES=`cd ${TEST_ROOT}; find . -mindepth 1 -type d | grep -v autom4te.cache | grep -v .svn | grep -v '.deps' | grep -v '.libs' | grep -v 'testCeylan' `

# "[OK] " is 5 character wide and must be aligned along the right edge :
COLUMNS=`tput cols`
space_for_test_name=`echo $(( $COLUMNS - 5 ))`

test_count=0
error_count=0

for m in ${TESTED_ROOT_MODULES} ; do
	
	printColor "\n${term_offset}${term_primary_marker}Playing all tests of module '"`echo $m | sed 's|^./||1'`"' : " $magenta_text $black_back
	
	TESTS=`find ${TEST_ROOT}/$m -mindepth 1 -maxdepth 1 -perm +o+x,g+x -a -type f -a -name 'testCeylan*' `
	if [ "$is_batch" == "1" ] ; then
	
		for t in $TESTS ; do
			if [ -x "$t" -a -f "$t" ] ; then
				printColor "${term_offset}${term_offset}+ `basename $t`" $cyan_text $black_back
			fi
		done

		echo -e "\n   <Press enter to start testing module '"`echo $m | sed 's|./||'`"'>"
		read $dummy
		clear
	fi

	for t in $TESTS ; do
		if [ -x "$t" -a -f "$t" ] ; then
		
			test_count=$(($test_count+1))
			test_name=`basename $t`
			
			if [ "$is_batch" == "1" ] ; then
				printColor "${term_primary_marker}Launching $test_name" $cyan_text $blue_back
			else
				printf "[${cyan_text}m%-${space_for_test_name}s" `echo $test_name|sed 's|^./||'`
			fi
				
			# The --interactive parameter is used to tell the test it is 
			# run in interactive mode, so that those which are long 
			# (ex : stress tests) are shorten.
			if [ "$is_batch" == "0" ] ; then
				./$t --batch 1>${TESTLOGFILE} 2>&1
			else
				./$t --interactive
			fi			
		
			return_code="$?"
			if [ "$return_code" == 0 ] ; then
				# Test succeeded :
				if [ "$is_batch" == "1" ] ; then
					echo
					printColor "${term_offset}$test_name seems to be successful     " $green_text $black_back
				else
					printf  "[${white_text}m[[${green_text}mOK[${white_text}m]\n"
				fi	
				
			else
				# Test failed :
				error_count=$(($error_count+1))
				if [ "$is_batch" == "1" ] ; then
					echo
					printColor "${term_offset}$t seems to be failed (exit status $return_code)     " $white_text $red_back
				else
					printf "[${white_text}m[[${red_text}mKO[${white_text}m]\n"
				
				fi	
			fi
		
			if [ "$is_batch" == "1" ] ; then
				printColor "${term_primary_marker}End of $test_name, press enter to continue" $cyan_text $blue_back
				read 
				clear
			fi	
		fi
			
	done
		
done 

echo 

if [ "$error_count" -eq "0" ] ; then
	echo "   Test result : [${green_text}m all $test_count tests succeeded[${white_text}m"
else
	echo "   Test result : [${red_text}m $error_count out of $test_count tests failed[${white_text}m"
fi	

echo -e "\nEnd of tests"

exit $error_count

