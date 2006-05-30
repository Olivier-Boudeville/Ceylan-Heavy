#!/bin/bash


SAGE="Usage : "`basename $0`" <module.py> [--debug]  : tests all available python modules.\nEx : "`basename $0`


if [ "$1" == "--debug" ] ; then
	do_debug=true
else
	do_debug=false
fi


TESTED_MODULE_PATH=`dirname $(pwd)`

export PYTHONPATH=${TESTED_MODULE_PATH}:$PYTHONPATH


function DEBUG
# Displays a debug message if debug mode is activated (do_debug=true).
# Usage : DEBUG "message 1" "message 2" ...
{
	[ "$do_debug" == "false" ] || echo "Debug : $*"
}

echo

echo "Testing all :"



for f in $TESTED_MODULE_PATH/tests/test-*.py; do

	echo 
	echo "Testing $f :"
	$f

done


echo "End of all tests"
