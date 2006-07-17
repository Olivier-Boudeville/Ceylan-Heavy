#!/bin/bash

# This script is a necessary special case since the server must be run
# before the client is launched.

#echo "playTests-local.sh sourced"

server_test_name="testCeylanServerStream"
client_test_name="testCeylanClientStream"

server_test_exec="${TEST_ROOT}/$m/${server_test_name}"
client_test_exec="${TEST_ROOT}/$m/${client_test_name}"

# Have to be excluded from automatic selection since must be managed
# specifically here :
EXCLUDED_TESTS="${server_test_name} ${client_test_name}"

# First, launch the server :

t=${server_test_exec}
if [ "$is_batch" == "0" ] ; then
	echo -e "\n\n########### Running now $t" >>${TESTLOGFILE}
	$t --batch 1>>${TESTLOGFILE} 2>&1 &
	SERVER_PID="$!"
else
	$t --interactive &
	SERVER_PID="$!"
fi			


# Then do as if client test was a classical test :
display_launching ${client_test_name}
run_test ${client_test_name} ${client_test_exec}

# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"

display_launching ${server_test_name}
display_test_result "${server_test_name}" "${server_test_exec}" "$return_code"

test_count=$(($test_count+2))

