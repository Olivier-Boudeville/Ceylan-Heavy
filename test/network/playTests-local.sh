# Not designed to be executed, must be sourced.

# This script is a necessary special case since the server must be run
# before the client is launched.

# One can ensure with : 
# ps -edf|grep testCeylanServer
#   - and - 
# netstat --tcp --listening |grep 6969
# 
# that no Ceylan server is already running, which may block the test.
#

DEBUG_INTERNAL "playTests-local.sh sourced"




sequential_server_test_name="testCeylanSequentialServerStream"
multiplexed_server_test_name="testCeylanMultiplexedServerStream"
client_test_name="testCeylanClientStream"

sequential_server_test_exec="${TEST_ROOT}/$m/${sequential_server_test_name}"
multiplexed_server_test_exec="${TEST_ROOT}/$m/${multiplexed_server_test_name}"
client_test_exec="${TEST_ROOT}/$m/${client_test_name}"


# Have to be excluded from automatic selection since must be managed
# specifically here :
EXCLUDED_TESTS="${sequential_server_test_name} ${multiplexed_server_test_name} ${client_test_name}"




### Testing sequential server :


# First, launch the server :

DEBUG_INTERNAL "Will launch the server"
t=${sequential_server_test_exec}
if [ "$is_batch" = "0" ] ; then
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

DEBUG_INTERNAL "Will launch the client"
run_test ${client_test_name} ${client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${sequential_server_test_name}
display_test_result "${sequential_server_test_name}" "${sequential_server_test_exec}" "$return_code"

test_count=$(($test_count+2))





### Testing multiplexed server :


multiplexed_server_test_exec="${TEST_ROOT}/$m/${multiplexed_server_test_name}"
client_test_exec="${TEST_ROOT}/$m/${client_test_name}"


# First, launch the server :

DEBUG_INTERNAL "Will launch the server"
t=${multiplexed_server_test_exec}
if [ "$is_batch" = "0" ] ; then
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

DEBUG_INTERNAL "Will launch the client"
run_test ${client_test_name} ${client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result :
wait ${SERVER_PID} 
return_code="$?"
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${multiplexed_server_test_name}
display_test_result "${multiplexed_server_test_name}" "${multiplexed_server_test_exec}" "$return_code"

# test client already counted :
test_count=$(($test_count+1))


