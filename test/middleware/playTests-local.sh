#!/bin/bash

# This script is a necessary special case since the server must be run
# before the client is launched.

# One can ensure with : 
# ps -edf|grep testCeylan
#   - and - 
# netstat --tcp --listening |grep 6969
# 
# that no Ceylan server is already running, which may block the test.
#

#echo "playTests-local.sh sourced"




marshalled_server_test_name="testCeylanMultiLwMarshalledServer"
marshalled_server_test_exec="${TEST_ROOT}/$m/${marshalled_server_test_name}"

# The classical network client does the job :
marshalled_client_test_name="testCeylanClientStream"
marshalled_client_test_exec="${TEST_ROOT}/network/${marshalled_client_test_name}"


multiplexed_lw_protocol_server_test_name="testCeylanMultiLwProtocolServer"
multiplexed_lw_protocol_server_test_exec="${TEST_ROOT}/$m/${multiplexed_lw_protocol_server_test_name}"

multiplexed_lw_protocol_client_test_name="testCeylanProtocolClient"
multiplexed_lw_protocol_client_test_exec="${TEST_ROOT}/$m/${multiplexed_lw_protocol_client_test_name}"


# Have to be excluded from automatic selection since must be managed
# specifically here :
EXCLUDED_TESTS="${marshalled_server_test_name} ${multiplexed_lw_protocol_server_test_name} ${multiplexed_lw_protocol_client_test_name}"




### Testing marshalled server :


# First, launch the server :

#echo "Will launch the server"
t=${marshalled_server_test_exec}
if [ "$is_batch" == "0" ] ; then
	echo -e "\n\n########### Running now $t" >>${TESTLOGFILE}
	$t --batch 1>>${TESTLOGFILE} 2>&1 &
	SERVER_PID="$!"
else
	$t --interactive &
	SERVER_PID="$!"
fi			


# Then do as if client test was a classical test :
display_launching ${marshalled_client_test_name}

# Ensure that the server is ready before the client :
sleep 1

#echo "Will launch the client"
run_test ${marshalled_client_test_name} ${marshalled_client_test_exec}

#echo "Wait for the server"
# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"
#echo "Server return code is $return_code"

display_launching ${marshalled_server_test_name}
display_test_result "${marshalled_server_test_name}" "${marshalled_server_test_exec}" "$return_code"

# Client already tested :
test_count=$(($test_count+1))





### Testing protocol server :


# First, launch the server :

#echo "Will launch the server"
t=${multiplexed_lw_protocol_server_test_exec}
if [ "$is_batch" == "0" ] ; then
	echo -e "\n\n########### Running now $t" >>${TESTLOGFILE}
	$t --batch 1>>${TESTLOGFILE} 2>&1 &
	SERVER_PID="$!"
else
	$t --interactive &
	SERVER_PID="$!"
fi			


# Then do as if client test was a classical test :
display_launching ${multiplexed_lw_protocol_client_test_name}

# Ensure that the server is ready before the client :
sleep 1

#echo "Will launch the client"
run_test ${multiplexed_lw_protocol_client_test_name} ${multiplexed_lw_protocol_client_test_exec}

#echo "Wait for the server"
# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"
#echo "Server return code is $return_code"

display_launching ${multiplexed_lw_protocol_server_test_name}
display_test_result "${multiplexed_lw_protocol_server_test_name}" "${multiplexed_server_test_exec}" "$return_code"

# test client not already counted :
test_count=$(($test_count+1))


