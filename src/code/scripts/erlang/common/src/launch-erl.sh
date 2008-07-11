#!/bin/sh

DEFAULT_NODE_NAME="ceylan_default"
DEFAULT_COOKIE="ceylan"


USAGE="`basename $0` [-v] [-c a_cookie] [--sn a_short_node_name | --ln a_long_node_name] [--fqdn a_fqdn] [--beam-dir a_path] [--beam-paths path_1 path_2] [--no-auto-start] [-h]...: launches the Erlang interpreter with specified settings.
	-v: be verbose
	-c a_cookie: specify a cookie
	--sn a_short_node_name: specify a short name (ex: 'ceylan_2') 
	--ln a_long_node_name: specify a long name (@FQDN will be automatically added)
	--fqdn a_fqdn: specify the FQDN to be used
	--wooper-path wooper_path: specify the WOOPER path 
	--eval 'an Erlang expression': start by evaluating this expression
	--beam-dir a_path: adds specified directory to the path searched for beam files (multiple --beam-dir options can be specified)
	--beam-paths first_path second_path ...: adds specified directories to the path searched for beam files (multiple paths can be specified; must be the last option)
	-h: display this help
	other options will be passed 'as are' to the interpreter
Unless --sn or --ln is specified, default is to use a long node name, '${DEFAULT_NODE_NAME}'.
	Example: launch-erl.sh -v --ln ceylan --eval 'class_TimeManager_test:run()'"	

#echo "Received as parameters: $*"

#ERL=/usr/bin/erl 
ERL=erl

CEYLAN_ERLANG=`dirname $0`/../..
#echo "CEYLAN_ERLANG = ${CEYLAN_ERLANG}"


# If logs are redirected to file:
DEFAULT_LOG_FILE="Ceylan-Simulation.log"


be_verbose=1

autostart=0

while [ $# -gt 0 ] ; do
	token_eaten=1
	
	if [ "$1" = "-v" ] ; then
		be_verbose=0
		token_eaten=0
	fi
	
	if [ "$1" = "-c" ] ; then
		shift
		COOKIE="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--sn" ] ; then
		shift
		SHORT_NAME="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--ln" ] ; then
		shift
		LONG_NAME="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--fqdn" ] ; then
		shift
		FQDN="$1"
		token_eaten=0
	fi
	
	if [ "$1" = "--beam-paths" ] ; then
		# Keep --beam-paths if first position, as will be shifted in end of loop
		while [ ! $# -eq 1 ] ; do
			#echo "  + adding beam path $2"
			CODE_DIRS="${CODE_DIRS} $2"
			shift
		done
		token_eaten=0
	fi
	
	if [ "$1" = "--eval" ] ; then
		shift
		TO_EVAL="-eval $1"
		EVAL_CONTENT="$1"
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
		CODE_DIRS="${CODE_DIRS} $1"
		token_eaten=0
	fi

	# Avoids warning of next test:
	if [ "$1" = "--batch" ] ; then
		VERBATIM_OPT="${VERBATIM_OPT} $1"
		token_eaten=0
	fi

	if [ $token_eaten -eq 1 ] ; then
		echo "Warning, unknown argument ($1), adding it 'as is' to command-line." 1>&2
		VERBATIM_OPT="${VERBATIM_OPT} $1"
	fi	
	
	shift
done


#+W w : log warnings as warnings.
#LOG_OPT="+W w -kernel error_logger "{file,\"$DEFAULT_LOG_FILE\"}
LOG_OPT="+W w"

# +native not used here:
CODE_OPT="-pz ${CODE_DIRS} -smp auto +K true +A 8"

# By default up to 1,2 million processes could be created on one node:
# (reduced, as even without having spawned these processes, the memory 
# footprint can increase quite a lot) 
MAX_PROCESS_COUNT=120000
#MAX_PROCESS_COUNT=120000000

COMMAND="${ERL} ${LOG_OPT} ${CODE_OPT} +P ${MAX_PROCESS_COUNT} ${VERBATIM_OPT}"

if [ -z "${COOKIE}" ] ; then
	COOKIE=${DEFAULT_COOKIE}
fi

if [ ${autostart} -eq 1 ] ; then
	echo " ** No autostart wanted, but you can run manually: ${EVAL_CONTENT}. **"
	TO_EVAL="-eval io:format(\"-->"${EVAL_CONTENT}".\")"
fi

COMMAND="${COMMAND} -setcookie ${COOKIE} ${TO_EVAL}"

# nslookup could be used as well:
# (some laptops timeout when using the 'host' command)
if [ -z "${FQDN}" ] ; then
	FQDN=`host \`hostname\` | awk '{ print $1 }' | head -n 1`
fi


if [ -n "${SHORT_NAME}" ] ; then

	if [ -z "${LONG_NAME}" ] ; then
		COMMAND="${COMMAND} -sname ${SHORT_NAME}"
	else
		echo "Error, --sn and --ln cannot be used simultaneously." 1>&2
		exit 1
	fi	

	if [ $be_verbose -eq 0 ] ; then
		echo "Launching: ${COMMAND}"
	else
		echo "Launching Erlang interpreter with short name ${SHORT_NAME}"
	fi
	
else

	if [ -z "${LONG_NAME}" ] ; then
		LONG_NAME="${DEFAULT_NODE_NAME}"
	fi
	
	LONG_NAME="${LONG_NAME}@${FQDN}"
	COMMAND="${COMMAND} -name ${LONG_NAME}"

	if [ $be_verbose -eq 0 ] ; then
		echo "Launching: ${COMMAND}"
	else
		echo "Launching Erlang interpreter with long name ${LONG_NAME}"
	fi
	
fi

#echo "$0 running final command: ${COMMAND}" 

${COMMAND}

