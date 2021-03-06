# Not designed to be executed, must be sourced.

# This script is a necessary special case since the server must be run before
# the client is launched.

# One can ensure with:
# ps -edf|grep testCeylanServer
#   - and -
# netstat --tcp --listening |grep 6969
#
# that no Ceylan server is already running, which may block the test.
#

DEBUG_INTERNAL "playTests-local.sh sourced"

# We chose to keep here the classical log plug, so that log writing remains
# immediate and not deferred as with the HTML plug.



sequential_server_test_name="testCeylanSequentialSrvStream"
multiplexed_server_test_name="testCeylanMultiplexedSrvStream"
client_test_name="testCeylanClientStream"

if [ $on_cygwin -eq 0 ] ; then

sequential_server_test_exec="${test_root}/$m/$m-${sequential_server_test_name}.exe"
multiplexed_server_test_exec="${test_root}/$m/$m-${multiplexed_server_test_name}.exe"
client_test_exec="${test_root}/$m/$m-${client_test_name}.exe"

else

sequential_server_test_exec="${test_root}/$m/${sequential_server_test_name}.exe"
multiplexed_server_test_exec="${test_root}/$m/${multiplexed_server_test_name}.exe"
client_test_exec="${test_root}/$m/${client_test_name}.exe"

fi


# Have to be excluded from automatic selection since must be managed
# specifically here:
excluded_tests="${sequential_server_test_name} ${multiplexed_server_test_name} ${client_test_name}"




### Testing sequential server:

check_no_ceylan_server_running

# First, launch the server:

DEBUG_INTERNAL "Will launch the server (1)"
t=${sequential_server_test_exec}
if [ $is_batch -eq 0 ] ; then
	echo "

	########### Running now $t" >>${test_log_file}
	echo "Command line: $t --batch ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --batch ${network_option} ${log_plug_option} 1>>${test_log_file} 2>&1 &
	server_pid=$!
else
	echo "Command line: $t --interactive ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --interactive ${network_option} ${log_plug_option} &
	server_pid=$!
fi

# Then do as if client test was a classical test:
display_launching ${client_test_name}

# Ensure that the server is ready before the client:
sleep 1

DEBUG_INTERNAL "Will launch the client"
run_test ${client_test_name} ${client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result:
wait ${server_pid}
return_code=$?
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${sequential_server_test_name}
display_test_result "${sequential_server_test_name}" "${sequential_server_test_exec}" "$return_code"

test_count=`expr $test_count + 2`





### Testing multiplexed server:

check_no_ceylan_server_running

# First, launch the server:

DEBUG_INTERNAL "Will launch the server (2)"
t=${multiplexed_server_test_exec}
if [ $is_batch -eq 0 ] ; then
	echo "

	########### Running now $t" >>${test_log_file}
	echo "Command line: $t --batch ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --batch ${network_option} ${log_plug_option} 1>>${test_log_file} 2>&1 &
	server_pid=$!
else
	echo "Command line: $t --interactive ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --interactive ${network_option} ${log_plug_option} &
	server_pid=$!
fi


# Then do as if client test was a classical test:
display_launching ${client_test_name}

# Ensure that the server is ready before the client:
sleep 1

DEBUG_INTERNAL "Will launch the client"
run_test ${client_test_name} ${client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result:
wait ${server_pid}
return_code=$?
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${multiplexed_server_test_name}
display_test_result "${multiplexed_server_test_name}" "${multiplexed_server_test_exec}" "$return_code"

# test client already counted:
test_count=`expr $test_count + 1`
