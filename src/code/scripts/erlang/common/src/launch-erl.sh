#!/bin/sh

default_node_name="ceylan_default"

# Not used anymore as the user may prefer file-based cookies:
#DEFAULT_cookie="ceylan"


USAGE="`basename $0` [-v] [-c a_cookie] [--sn a_short_node_name | --ln a_long_node_name] [--fqdn a_fqdn] [--beam-dir a_path] [--beam-paths path_1 path_2] [--no-auto-start] [-h]...: launches the Erlang interpreter with specified settings.
	-v: be verbose
	-c a_cookie: specify a cookie, otherwise no cookie will be specifically set
	--sn a_short_node_name: specify a short name (ex: 'ceylan_2') 
	--ln a_long_node_name: specify a long name (@FQDN will be automatically added)
	--tcp-range LOW HIGH: specify the range of TCP ports that should be used
	--fqdn a_fqdn: specify the FQDN to be used
	--background: run the launched interpreter in the background
	--wooper-path wooper_path: specify the WOOPER path 
	--eval 'an Erlang expression': start by evaluating this expression
	--beam-dir a_path: adds specified directory to the path searched for beam files (multiple --beam-dir options can be specified)
	--beam-paths first_path second_path ...: adds specified directories to the path searched for beam files (multiple paths can be specified; must be the last option)
	-h: display this help
	other options will be passed 'as are' to the interpreter
Unless --sn or --ln is specified, default is to use a long node name, '${default_node_name}'.
	Example: launch-erl.sh -v --ln ceylan --eval 'class_TimeManager_test:run()'"	

#echo "Received as parameters: $*"

#ERL=/usr/bin/erl 
ERL=erl

CEYLAN_ERLANG=`dirname $0`/../..
#echo "CEYLAN_ERLANG = ${CEYLAN_ERLANG}"


# If logs are redirected to file:
DEFAULT_LOG_FILE="Ceylan-run.log"

be_verbose=1
use_tcp_range=1
autostart=0
in_background=1


while [ $# -gt 0 ] ; do
	token_eaten=1
	
	#echo "Examining next argument: '$1'"
	
	if [ "$1" = "-v" ] ; then
		be_verbose=0
		token_eaten=0
	fi
	
	if [ "$1" = "-c" ] ; then
		shift
		#echo "  + specified cookie: $cookie"		
		cookie="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--sn" ] ; then
		shift
		short_name="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--ln" ] ; then
		shift
		long_name="$1"
		token_eaten=0
	fi

	if [ "$1" = "--tcp-range" ] ; then
		shift
		use_tcp_range=0
		lower_tcp_port="$1"
		higher_tcp_port="$2"
		shift
		# Already done at the end of the loop: shift
		#echo "  + TCP range: from $lower_tcp_port to $higher_tcp_port"
		token_eaten=0
	fi
	
	if [ "$1" = "--epmd_port" ] ; then
		shift
		epmd_port="$1"
		# This is apparently the way to notify a VM of the EPMD port:
		export ERL_EPMD_PORT=$epmd_port
		token_eaten=0
	fi
	
	if [ "$1" = "--fqdn" ] ; then
		shift
		fqdn="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--background" ] ; then
		shift
		in_background=0
		token_eaten=0
	fi
	
	if [ "$1" = "--beam-paths" ] ; then
		# Keep --beam-paths if first position, as will be shifted in end of loop
		while [ ! $# -eq 1 ] ; do
			#echo "  + adding beam path $2"
			code_dirs="${code_dirs} $2"
			shift
		done
		token_eaten=0
	fi
	
	if [ "$1" = "--eval" ] ; then
		shift
		# We can use -s instead, which would allow to send multiple commands
		# in a row.
		TO_EVAL="-eval $1"
		eval_content="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--no-auto-start" ] ; then
		#echo "Autostart deactivated"
		autostart=1
		token_eaten=0
	fi

	if [ "$1" = "-h" ] ; then
		echo -e "$USAGE"
		exit
		token_eaten=0
	fi

	if [ "$1" = "--beam-dir" ] ; then
		shift
		#echo "  + adding beam dir $1"
		code_dirs="${code_dirs} $1"
		token_eaten=0
	fi

	# Avoids warning of next test:
	if [ "$1" = "--batch" ] ; then
		verbatim_opt="${verbatim_opt} $1"
		token_eaten=0
	fi

	if [ $token_eaten -eq 1 ] ; then
		echo "Warning, unknown argument ('$1'), adding it 'as is' to command-line." 1>&2
		verbatim_opt="${verbatim_opt} $1"
	fi	
	
	shift
done


#+W w : log warnings as warnings.
#log_opt="+W w -kernel error_logger "{file,\"$DEFAULT_LOG_FILE\"}
log_opt="+W w"

# +native not used here:
code_opt="-pz ${code_dirs} -smp auto +K true +A 8"

# By default up to 1,2 million processes could be created on one node:
# (reduced, as even without having spawned these processes, the memory 
# footprint can increase quite a lot) 
max_process_count=120000
#max_process_count=120000000


command="${ERL} ${log_opt} ${code_opt} +P ${max_process_count} ${verbatim_opt}"

# Adds a command-line cookie only if specified:
if [ -n "${cookie}" ] ; then
	cookie_opt="-setcookie ${cookie}"
fi

if [ ${autostart} -eq 1 ] ; then
	echo " ** No autostart wanted, but you can run manually: ${eval_content}. **"
	TO_EVAL="-eval io:format(\"-->"${eval_content}".\")"
fi


if [ $use_tcp_range -eq 0 ] ; then

	tcp_port_opt="-kernel inet_dist_listen_min ${lower_tcp_port} inet_dist_listen_max ${higher_tcp_port}"  

fi

command="${command} ${cookie_opt} ${TO_EVAL} ${tcp_port_opt}"



# nslookup could be used as well:
# (some laptops timeout when using the 'host' command)
if [ -z "${fqdn}" ] ; then
	# Not used anymore:
	fqdn=`host \`hostname\` | awk '{ print $1 }' | head -n 1`
	#echo "Guessed FQDN is ${fqdn}"
fi


if [ -n "${short_name}" ] ; then

	if [ -z "${long_name}" ] ; then
		command="${command} -sname ${short_name}"
	else
		echo "Error, --sn and --ln cannot be used simultaneously." 1>&2
		exit 1
	fi	

	if [ $be_verbose -eq 0 ] ; then
		echo "Launching: ${command}"
	else
		echo "Launching Erlang interpreter with short name ${short_name}"
	fi
	
else

	if [ -z "${long_name}" ] ; then
		long_name="${default_node_name}"
	fi
	
	#long_name="${long_name}@${fqdn}"
	command="${command} -name ${long_name}"

	if [ $be_verbose -eq 0 ] ; then
		#echo "Launching: ${command}"
		dummy=1
	else
		echo "Launching Erlang interpreter with long name ${long_name}"
	fi
	
fi



if [ $in_background -eq 0 ] ; then
	background_opt="-noinput -noshell -detached"
fi

command="${command} ${background_opt}"

# Uncomment to see the actual runtime settings:
#echo "$0 running final command: ${command}" 

${command}
pid=$!

# Commented out, as pid never set:
#if [ $in_background -eq 0 ] ; then
#	echo "(PID of launched interpreter is $pid)"
#fi

