#!/bin/sh


test_log_file=`pwd`"/testsOutcome.txt"
playtest_local_file="playTests-local.sh"


USAGE="`basename $0` [--interactive]: executes all tests for Ceylan in a row.

	If the --interactive option is used, tests will not be run in batch mode, and will prompt the user for various inputs. Otherwise only their final result will be output. In all cases their messages will be stored in file ${test_log_file}. The return code of this script will be the number of failed tests (beware to overflow of the return code)"


# Remember, when debugging on UNIX playTests.sh, to execute it from
# *installed* version, but to modify the playTests.sh from *source* code,
# and to copy back the latter to the former.


# In batch (non-interactive) mode by default (0):
is_batch=0



if [ $# -ge 2 ] ; then
	echo "
	Usage: $USAGE" 1>&2
	exit 1
fi


if [ $# -eq 1 ] ; then
	if [ "$1" != "--interactive" ] ; then
		echo "$1: unknown option." 1>&2
		echo "
		Usage: $USAGE" 1>&2
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
	[ $debug_mode -eq 1 ] || echo "Debug: $*"
}


WARNING_INTERNAL()
# Prints warning informations to error output.
{
	echo "Warning: $*" 1>&2
}


ERROR_INTERNAL()
# Prints error informations to error output.
{
	echo "Error: $*" 1>&2
}


DEBUG_INTERNAL "Debug mode activated"



# Some useful test functions:

display_launching()
# Usage: display_launching <name of the test>
{

	test_name="$1"

	if [ $is_batch -eq 1 ] ; then
		printColor "${term_primary_marker}Launching $test_name" $cyan_text $blue_back
	else
		printf "[${cyan_text}m%-${space_for_test_name}s" `echo $test_name|sed 's|^./||'`
	fi

}



display_test_result()
# Usage: display_test_result <name of the test> <test path> <returned code>
{

	test_name="$1"
	t="$2"
	return_code="$3"

	if [ "$return_code" -eq 0 ] ; then
		# Test succeeded:
		if [ $is_batch -eq 1 ] ; then
			echo
			printColor "${term_offset}$test_name seems to be successful     " $green_text $black_back
		else
			printf  "[${white_text}m[[${green_text}mOK[${white_text}m]\n"
		fi

	else

		# Test failed:
		error_count=`expr $error_count + 1`
		if [ $is_batch -eq 1 ] ; then
			echo
			printColor "${term_offset}$t seems to be failed (exit status $return_code)     " $white_text $red_back
		else

			if [ $check_dependency -eq 0 ] ; then

				if [ $on_cygwin -eq 0 ] ; then
					# See also: http://www.dependencywalker.com/
					PATH="/cygdrive/c/Program Files/Microsoft Platform SDK for Windows Server 2003 R2/bin:$PATH"
					depend_tool="Depends.exe"
					if which $depend_tool 1>/dev/null 2>&1; then
						depends_exe=`which $depend_tool`
						"$depends_exe" $t >> ${test_log_file}
					else
						echo "No $depend_tool available, no dependency displayed."  >> ${test_log_file}
					fi
				else
					if [ $has_ldd -eq 0 ] ; then
						echo "$t failed, whose shared library dependencies are: " >> ${test_log_file}
						${ldd_tool} $t >>${test_log_file}
					else
						echo "$t failed." >> ${test_log_file}
					fi
				fi
			fi

			printf "[${white_text}m[[${red_text}mKO[${white_text}m]\n"
		fi
	fi
}



run_test()
# Usage: run_test <name of the test> <test path>
{

	test_name="$1"
	t="$2"

	# The --interactive parameter is used to tell the test it is
	# run in interactive mode, so that those which are long
	# (ex: stress tests) are shorten.

	echo "

	########### Running now $t" >>${test_log_file}

	if [ $has_ldd -eq 0 ] ; then
		echo "Library dependencies: " >>${test_log_file}
		${ldd_tool} $t >>${test_log_file}
	fi

	if [ $is_batch -eq 0 ] ; then

		echo "Command line: $t --batch ${network_option} ${log_plug_option}" >>${test_log_file}

		$t --batch ${network_option} ${log_plug_option} 1>>${test_log_file} 2>&1

	else

		echo "Command line: $t --interactive ${network_option} ${log_plug_option}" >>${test_log_file}
		$t --interactive ${network_option} ${log_plug_option}

	fi

	return_code="$?"

	display_test_result "$test_name" "$t" "$return_code"


	if [ $return_code -eq 0 ] ; then
		echo "Test $t succeeded." 1>>${test_log_file}
	else
		echo "Test $t failed with return code $return_code." 1>>${test_log_file}
	fi


	if [ $is_batch -eq 1 ] ; then

		printColor "${term_primary_marker}End of $test_name, press enter to continue" $cyan_text $blue_back
		read
		clear

	fi

}



display_final_stats()
{

	echo

	if [ $error_count -eq 0 ] ; then
		echo "   Test result: [${green_text}m all $test_count tests succeeded[${white_text}m"
	else
		echo "   Test result: [${red_text}m $error_count out of $test_count tests failed[${white_text}m"
		echo "   (see ${test_log_file} for more details)"
	fi

}



get_logical_test_name()
# Converts executables names on Cygwin to the usual names
# 'system-testCeylanX.exe' should become 'testCeylanX'
{

	if [ $on_cygwin -eq 0 ] ; then
		returned_string=`basename $t |sed 's|^.*-test|test|1' |sed 's|.exe$||1'`
	else
		returned_string=`basename $t |sed 's|.exe$||1'`
	fi

}


check_no_ceylan_server_running()
# Check that there are no pending servers that might interfere with tests.
{

	hanging_tests=`ps -ef|grep testCeylan|grep '\.exe'|grep -v 'grep testCeylan'|awk '{print $8}'|sed 's|.*/||1'`

	if [ -n "${hanging_tests}" ] ; then

		# User might be looking at the HTML log output, that's ok:
		if [ ! ${hanging_tests} = "firefox" ] ; then

			ERROR_INTERNAL "apparently there is at least one hanging test before the test suite is launched, please remove it so that it cannot interfere with these new tests (detected: ${hanging_tests})"
			exit 10

		fi

	fi

}


# Try to find shell utilities:

test_root=`dirname $0`
# For testCeylanFileLocator and others that need to search relative paths:
# (do not know why shell fails when doing a 'cd test' when run from trunk)
cd ${test_root}

shell_location="${test_root}/../src/code/scripts/shell"

# Triggers also termUtils.sh and platformDetection.sh:
default_locations_path="$shell_location/defaultLocations.sh"

if [ -f "$default_locations_path" ] ; then
	. $default_locations_path
else

	shell_location="${test_root}/../Ceylan/scripts/shell"
	default_locations_path="$shell_location/defaultLocations.sh"

	if [ -f "$default_locations_path" ] ; then
		. $default_locations_path
	else

		ERROR_INTERNAL "default location script not found (tried finally $default_locations_path)"
		exit 3
	fi
fi

# For ping:
findSupplementaryShellTools

check_no_ceylan_server_running

# Creates a test directory to avoid polluting other directories:
TEST_DIR="tests-results-"`date '+%Y%m%d'`


# Specifies the log plug the tests should be run with.
# Note: avoid using the classical plug because it may cause scheduling
# failures (ex: with testOSDLScheduler) because of its default synchronous
# file I/O).
log_plug_option="--HTMLPlug"


if [ $is_batch -eq 0 ] ; then
	echo "
  Running in batch mode, tests will be short and silent, only results are to be output."
else
	echo "
  Interactive tests will only need the enter key to be pressed one or more times. Be warned though that some tests might take a long time, and that some of them have no special output except a test result."
fi



# Apparently the ping utility provided by Cygwin is not able to send a
# given number of packets (neither '-n' nor '-c' is working):
if [ $use_cygwin -eq 1 ] ; then

	# Not on Cygwin, testing whether we are online (needed for DNS queries):
	if ${PING} ${PING_OPT} 2 google.com 1>/dev/null 2>&1; then
		is_online=0
		network_option="--online"
		echo "
  Running in online mode, in-depth network testing enabled."
	else
		is_online=1
		network_option=""
		echo "
			No Internet connection detected, some network tests will be disabled."
	fi

else

	# Let's suppose that this Cygwin is connected to the Internet:
	is_online=0

	echo "
  Running in online mode, in-depth network testing enabled, supposing a direct connection to the Internet is available."

fi



test_count=0
error_count=0


# Not using Cygwin by default to chain the tests:
on_cygwin=1


# Tells whether link dependencies should be checked in case a test fails:
check_dependency=1


# Special case for tests generated on Windows:
if [ `uname -s | cut -b1-6` = "CYGWIN" ] ; then

	on_cygwin=0
	DEBUG_INTERNAL "Running tests in the Windows (Cygwin) context."

	# The tests and the Ceylan library they use will match if and only if they
	# all come from a vanilla Ceylan install or a LOANI-based one:
	# in the two cases the Ceylan library will have a specific name (either
	# with 'from-LOANI' or not), and the tests will expect that DLL name too.
	# Hence the order of following two paths does not matter:

	# Tests with LOANI are by default run against the debug multithread Ceylan
	# library:
	ceylan_library_from_loani_tested_flavour="debug-mt"

	# Updated PATH needed to find the Ceylan DLL in LOANI install tree:
	export PATH="../../../../../LOANI-installations/OSDL-libraries/${ceylan_library_from_loani_tested_flavour}/dll:$PATH"

	# Updated PATH needed to find the Ceylan DLL in Ceylan build tree:
	export PATH="../src/code:$PATH"

fi


# This script will automatically run each test of each selected Ceylan module.
tested_root_modules=`cd ${test_root}; find . -type d | grep -v cross-tests | grep -v tmp | grep -v Debug | grep -v autom4te.cache | grep -v .svn | grep -v '.deps' | grep -v '.libs' | grep -v 'testCeylan'| grep -v '^\.$'`

DEBUG_INTERNAL "Tested modules are: ${tested_root_modules}"


# "[OK] " is 5 character wide and must be aligned along the right edge:
if [ -z "${COLUMNS}" ] ; then
	DEBUG_INTERNAL "Retrieving columns thanks to tput"
	tput_exec=`which tput 2>/dev/null`
	if [ -x "${tput_exec}" ] ; then

		COLUMNS=`tput cols`

	fi
	if [ -z "${COLUMNS}" ] ; then
		DEBUG_INTERNAL "Retrieving columns thanks to stty"
		COLUMNS=`stty size |awk '{print $2}'`
	fi
fi
DEBUG_INTERNAL "Columns = ${COLUMNS}"

space_for_test_name=`expr ${COLUMNS} - 5`
DEBUG_INTERNAL "Space for test names = ${space_for_test_name}"


if [ $is_batch -eq 0 ] ; then
	echo "
			Test results established on "`LANG=C date '+%A, %B %-e, %Y'`"\n\n" > ${test_log_file}
fi

if [ $on_cygwin -eq 0 ] ; then
	echo "

			 Library search path is: PATH='$PATH'" >> ${test_log_file}
else
	echo "

			 Library search path is: LD_LIBRARY_PATH='$LD_LIBRARY_PATH'" >> ${test_log_file}
fi

has_ldd=1
ldd_tool=`which ldd 2>/dev/null`

if [ -x "${ldd_tool}" ] ; then
	has_ldd=0
fi

# So that test plugin can be found:
saved_ltdl_library_path="$LTDL_LIBRARY_PATH"


for m in ${tested_root_modules} ; do

	DEBUG_INTERNAL "Testing module ${m}"

	LTDL_LIBRARY_PATH="$saved_ltdl_library_path:${test_root}/$m"
	export LTDL_LIBRARY_PATH

	# Some local scripts are needed in some cases, for example when a test
	# client requires a test server to be launched before.

	playtest_local="${test_root}/$m/${playtest_local_file}"

	printColor "
	${term_offset}${term_primary_marker}Playing all tests of module '"`echo $m | sed 's|^./||1'`"': " $magenta_text $black_back

	if [ -f "${playtest_local}" ] ; then
		excluded_tests=""
		DEBUG_INTERNAL "Sourcing ${playtest_local}"
		. ${playtest_local}
	fi

	if [ $on_cygwin -eq 0 ] ; then
		TESTS=`ls ${test_root}/$m/*-testCeylan*.exe 2>/dev/null`
	else
		TESTS=`ls ${test_root}/$m/testCeylan*.exe 2>/dev/null`
	fi

	DEBUG_INTERNAL "Tests in module ${m} are: '${TESTS}'"

	if [ $is_batch -eq 1 ] ; then

		# Lists all tests that are to be run:
		for t in $TESTS ; do
			if [ -x "$t" -a -f "$t" ] ; then
				printColor "${term_offset}${term_offset}+ `basename $t`" $cyan_text $black_back
			fi
		done

		echo "
				<Press enter to start testing module '"`echo $m | sed 's|./||'`"'>"
		read $dummy
		clear
	fi

	for t in $TESTS ; do

		if [ -x "$t" -a -f "$t" ] ; then

			get_logical_test_name $t
			logical_test_name=$returned_string

			to_be_excluded=1
			for excluded in ${excluded_tests} ; do
				if [ "$logical_test_name" = "$excluded" ] ; then
					to_be_excluded=0
				fi
			done

			if [ $to_be_excluded -eq 0 ] ; then
				# Skip this test, as playtest_local_file took care of it:
				DEBUG_INTERNAL "Skipping test $t"
				continue
			fi

			test_count=`expr $test_count + 1`

			get_logical_test_name $t
			test_name="$returned_string"

			display_launching $test_name

			run_test $test_name "$t"

		fi

	done

done

display_final_stats


echo "
  End of tests"


LTDL_LIBRARY_PATH="$saved_ltdl_library_path"


exit $error_count
