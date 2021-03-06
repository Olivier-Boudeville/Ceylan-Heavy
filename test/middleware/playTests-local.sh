# Not designed to be executed, must be sourced.

# This script is a necessary special case since the server must be run before
# the client is launched.

# One can ensure with:
# ps -edf|grep testCeylan
#   - and -
# netstat --tcp --listening |grep 6969
#
# that no Ceylan server is already running, which may block the test.
#

DEBUG_INTERNAL "middleware/playTests-local.sh sourced"

# We chose to keep here the classical log plug, so that log writing remains
# immediate and not deferred as with the HTML plug.


# The classical network client 'testCeylanClientStream' will do the job:

marshalled_server_test_name="testCeylanMultiLwMrshSrv"
marshalled_client_test_name="testCeylanClientStream"
multiplexed_lw_protocol_server_test_name="testCeylanMultiLwPtclSrv"
multiplexed_lw_protocol_client_test_name="testCeylanProtocolClient"

if [ $on_cygwin -eq 0 ] ; then

	marshalled_server_test_exec="${test_root}/$m/$m-${marshalled_server_test_name}.exe"
	marshalled_client_test_exec="${test_root}/network/network-${marshalled_client_test_name}.exe"
	multiplexed_lw_protocol_server_test_exec="${test_root}/$m/$m-${multiplexed_lw_protocol_server_test_name}.exe"
	multiplexed_lw_protocol_client_test_exec="${test_root}/$m/$m-${multiplexed_lw_protocol_client_test_name}.exe"

else

	marshalled_server_test_exec="${test_root}/$m/${marshalled_server_test_name}.exe"
	marshalled_client_test_exec="${test_root}/network/${marshalled_client_test_name}.exe"
	multiplexed_lw_protocol_server_test_exec="${test_root}/$m/${multiplexed_lw_protocol_server_test_name}.exe"
	multiplexed_lw_protocol_client_test_exec="${test_root}/$m/${multiplexed_lw_protocol_client_test_name}.exe"

fi


# Have to be excluded from automatic selection since must be managed
# specifically here:
excluded_tests="${marshalled_server_test_name} ${multiplexed_lw_protocol_server_test_name} ${multiplexed_lw_protocol_client_test_name}"




### Testing marshalled server:

check_no_ceylan_server_running

# First, launch the server:

DEBUG_INTERNAL "Will launch the server"
t=${marshalled_server_test_exec}
if [ $is_batch -eq 0 ] ; then
	echo "

	########### Running now $t" >> ${test_log_file}
	echo "Command line: $t --batch ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --batch ${network_option} ${log_plug_option} 1>> ${test_log_file} 2>&1 &
	server_pid=$!
else
	echo "Command line: $t --interactive ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --interactive ${network_option} ${log_plug_option} &
	server_pid=$!
fi


# Then do as if client test was a classical test:
display_launching ${marshalled_client_test_name}

# Ensure that the server is ready before the client:
sleep 2

DEBUG_INTERNAL "Will launch the client"
run_test ${marshalled_client_test_name} ${marshalled_client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result:
wait ${server_pid}
return_code=$?
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${marshalled_server_test_name}
display_test_result "${marshalled_server_test_name}" "${marshalled_server_test_exec}" "$return_code"

# Client already tested:
test_count=`expr $test_count + 1`





### Testing protocol server:

check_no_ceylan_server_running

# First, launch the server:

DEBUG_INTERNAL "Will launch the server"
t=${multiplexed_lw_protocol_server_test_exec}
if [ $is_batch -eq 0 ] ; then

	echo "

	########### Running now $t" >> ${test_log_file}
	echo "Command line: $t --batch ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --batch ${network_option}  ${log_plug_option} 1>> ${test_log_file} 2>&1 &
	server_pid=$!

else

	echo "Command line: $t --interactive ${network_option} ${log_plug_option}" >>${test_log_file}
	$t --interactive ${network_option} ${log_plug_option} &
	server_pid=$!

fi


# Then do as if client test was a classical test:
display_launching ${multiplexed_lw_protocol_client_test_name}

# Ensure that the server is ready before the client:
sleep 1

DEBUG_INTERNAL "Will launch the client"
run_test ${multiplexed_lw_protocol_client_test_name} ${multiplexed_lw_protocol_client_test_exec}

DEBUG_INTERNAL "Wait for the server"
# Now inspect the server result:
wait ${server_pid}
return_code=$?
DEBUG_INTERNAL "Server return code is $return_code"

display_launching ${multiplexed_lw_protocol_server_test_name}
display_test_result "${multiplexed_lw_protocol_server_test_name}" "${multiplexed_server_test_exec}" "$return_code"

# test client not already counted:
test_count=`expr $test_count + 2`
