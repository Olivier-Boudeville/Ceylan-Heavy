#!/bin/sh

script_name="valgrindTest.sh"

run_dir=`dirname $0`
checker_script=`cd $run_dir; pwd`/../src/code/scripts/shell/${script_name}
#echo "checker_script = $checker_script"

if [ ! -x "${checker_script}" ] ; then

	echo "  Error, no executable Ceylan Valgrind script found (${script_name})." 1>&2
	exit 5

fi

# One may replace $run_dir by $run_dir/system for example, to perform per-module
# testings:
all_tests=`find $run_dir -type f -a -name 'testCeylan*.exe'`
#echo "all_tests = ${all_tests}"


# Skips tests that cannot run standalone:
#
# (one should run a valgrind'ed server and a valgrind'ed client each on separate
# consoles; for example: valgrindTest.sh testCeylanMultiLwPtclSrv.exe and then
# valgrindTest.sh testCeylanProtocolClient.exe on another shell)


target_tests=""

for t in ${all_tests} ; do

	case $t in

		# We filter-out servers (as they would freeze), but not clients, as it
		# allows use to test their failure code path ('connection refused',
		# since no server was launched) as well.

		# To be tested with testCeylanClientStream.exe:
		./network/testCeylanSequentialSrvStream.exe)
			;;

		./network/testCeylanMultiplexedSrvStream.exe)
			;;

		./middleware/testCeylanMultiLwMrshSrv.exe)
			;;

		# To be with testCeylanProtocolClient.exe:
		./middleware/testCeylanMultiLwPtclSrv.exe)
			;;


		*)
			target_tests="$target_tests $t"
			;;

	esac

done

#echo "target_tests = ${target_tests}"


if [ -z "${target_tests}" ] ; then

	echo "  Error, no target test found (were the tests built?)." 1>&2
	exit 6

fi

count=`echo ${target_tests} | wc -w`

echo "
Running Valgrind recursively on all $count Ceylan tests:
"


# Command-line option to run each test with:
run_opt="--batch"

for t in ${target_tests} ; do

	echo "   + checking test $t"

	${checker_script} $t ${run_opt}

	echo

done
