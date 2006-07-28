#!/bin/bash

# This script is a necessary special case since the server must be run
# before the client is launched.

# One can ensure with : 
# ps -edf|grep testCeylanServer
#   - and - 
# netstat --tcp --listening |grep 6969
# 
# that no Ceylan server is already running, which may block the test.
#

#echo "playTests-local.sh sourced"

server_test_name="testCeylanServerStream"
client_test_name="testCeylanClientStream"

server_test_exec="${TEST_ROOT}/$m/${server_test_name}"
client_test_exec="${TEST_ROOT}/$m/${client_test_name}"

# Have to be excluded from automatic selection since must be managed
# specifically here :
EXCLUDED_TESTS="${server_test_name} ${client_test_name}"

# First, launch the server :

#echo "Will launch the server"
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

# Ensure that the server is ready before the client :
sleep 1

#echo "Will launch the client"
run_test ${client_test_name} ${client_test_exec}

#echo "Wait for the server"
# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"
#echo "Server return code is $return_code"

display_launching ${server_test_name}
display_test_result "${server_test_name}" "${server_test_exec}" "$return_code"

test_count=$(($test_count+2))

