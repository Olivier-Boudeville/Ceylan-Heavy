#!/bin/bash

USAGE="Usage : "`basename $0`" <module.py> [--debug]  : tests python toolbox module provided as argument.\nEx : "`basename $0`" fileUtils.py"

echo

if [ "$#" -eq "0" ] ; then
	echo -e "Error, not enough arguments. $USAGE"
	exit 1
fi

shift	
  
if [ "$1" == "--debug" ] ; then
	do_debug=true
else
	do_debug=false
fi

TESTED_MODULE_PATH=`dirname $(pwd)`
TESTED_MODULE="fileUtils.py"
TESTING_MODULE="test-$TESTED_MODULE"
TEST_TARGET="$TESTED_MODULE_PATH/tests/$TESTING_MODULE"

if [ ! -f "$TEST_TARGET" ] ; then
	echo -e "Error, no test target available ($TEST_TARGET). $USAGE"
	exit 2
fi


function DEBUG
# Displays a debug message if debug mode is activated (do_debug=true).
# Usage : DEBUG "message 1" "message 2" ...
{
	[ "$do_debug" == "false" ] || echo "Debug : $*"
}



export PYTHONPATH=${TESTED_MODULE_PATH}:$PYTHONPATH

echo


if [ ! -f "$TEST_TARGET" ] ; then
	echo "Error, test target module (<$TEST_TARGET>) not found."
	exit 1 
fi

if [ ! -x "$TEST_TARGET" ] ; then
	echo "Error, test target module (<$TEST_TARGET>) is not executable."
	exit 2
fi

#DEBUG "Test target is <$TEST_TARGET>."


if [ "$do_debug" == "true" ] ; then
	python -i $TEST_TARGET
else
	$TEST_TARGET
fi

