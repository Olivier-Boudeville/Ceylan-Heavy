#!/bin/bash

USAGE="Usage :"`basename $0`"[--only-prepare-dist] [--ceylan-install-prefix <a prefix>] : (re)generates all the autotools-based build system for Ceylan tests.\n\t--only-prepare-dist : perform only necessary operations so that the test directory can be distributed afterwards\n\t--ceylan-install-prefix <a prefix> : use the specified prefix to find Ceylan installation"

# These tests must rely on a related Ceylan source directory, since they :
#	- need to know which Ceylan version is to be tested
#	- use some Ceylan facilities (ex : Ceylan substitute script)


# Main settings section.

# 0 means true, 1 means false :
do_remove_generated=0
do_stop_after_configure=1
do_clean=0
do_build=0
do_install=0
do_test=0


while [ "$#" -gt "0" ] ; do
	token_eaten=1
		
	if [ "$1" == "-o" ] || [ "$1" == "--only-prepare-dist" ] ; then
		do_stop_after_configure=0
		token_eaten=0
	fi
	
	if [ "$1" == "--ceylan-install-prefix" ] ; then
		shift
		library_location="$1"
		if [ ! -d "$ceylan_install_prefix" ] ; then
			echo -e "Error, specified prefix for Ceylan install ($ceylan_install_prefix) does not exist.\n$USAGE" 1>&2
			exit 10
		fi
		
		token_eaten=0
	fi
	
	if [ "$1" == "-h" ] || [ "$1" == "--help" ] ; then
		echo -e "$USAGE"
		exit
		token_eaten=0
	fi

	if [ "$token_eaten" == "1" ] ; then
		echo -e "Error, unknown argument ($1).\n$USAGE" 1>&2
		exit 4
	fi	
	shift
done


# debug mode activated iff equal to true (0) :
debug_mode=1

debug()
{
	if [ $debug_mode -eq 0 ] ; then
		echo "debug : $*"
	fi	
}



library_location_opt="--with-libCeylan=$library_location"
#library_location_opt=""

test_install_location="$library_location"
test_install_location_opt="--prefix=$test_install_location"

# To check the user can override them :
#test_overriden_options="CPPFLAGS=\"-DTEST_CPPFLAGS\" LDFLAGS=\"-LTEST_LDFLAGS\""
test_overriden_options=""

configure_opt="-enable-strict-ansi --enable-debug $library_location_opt $test_install_location_opt $test_overriden_options"


RM="/bin/rm -f"




# Wait-after-execution mode activated iff equal to true (0) :
wait_activated=1

wait()
{

	if [ $wait_activated -eq 0 ] ; then
		echo "  <press enter key to continue>"
		read
	fi	
	
}

cd `dirname $0`

# Searches for the Ceylan substitute script :
CEYLAN_SUBSTITUTE_SCRIPT="../src/code/scripts/shell/substitute.sh"
if [ ! -x "${CEYLAN_SUBSTITUTE_SCRIPT}" ] ; then
	CEYLAN_SUBSTITUTE_SCRIPT="../code/scripts/shell/substitute.sh"
	if [ ! -x "${CEYLAN_SUBSTITUTE_SCRIPT}" ] ; then
	echo "Error, no executable Ceylan substitute script found." 1>&2
	exit 1
	fi
fi

# Searches for the Ceylan settings file, both in SVN installs and releases :
CEYLAN_SETTINGS_FILE="../src/conf/CeylanSettings.inc"
if [ ! -f "${CEYLAN_SETTINGS_FILE}" ] ; then
	CEYLAN_SETTINGS_FILE="../conf/CeylanSettings.inc"
	if [ ! -f "${CEYLAN_SETTINGS_FILE}" ] ; then
		echo "Error, no Ceylan settings file found." 1>&2
	exit 2
	fi
fi


# Log-on-file mode activated iff equal to true (0) :
log_on_file=1

log_filename="autogen.log"

debug "log_filename = $log_filename"
if [ -f "$log_filename" ]; then
	$RM "$log_filename"
fi


# Overall autotools settings :

# Be verbose for debug purpose :
#verbose="--verbose"
verbose=""

# Copy files instead of using symbolic link :
copy="--copy"
#copy=""

# Replace existing files :
#force="--force"
force=""

# Warning selection : 
warnings="--warnings=all"
#warnings=""



echo
echo "Bootstrapping now build for test system thanks to the autotools"
echo "      (this may take a while ...)"
echo


execute()
{

	echo "    Executing $*"
	
	if [ "$log_on_file" -eq 0 ] ; then
		echo "    Executing $* from "`pwd` >>"$log_filename"
		eval $* >>"$log_filename" 2>&1
		RES=$?
		echo "----------------------------------------" >>"$log_filename"
		echo >>"$log_filename"
	else
		eval $* 
		RES=$?
	fi

	if [ ! $RES -eq 0 ] ; then
		echo 1>&2
		if [ "$log_on_file" -eq 0 ] ; then
			echo "Error while executing '$*', see $log_filename" 1>&2
		else
			echo "Error while executing '$*'" 1>&2
		fi
			
		exit $RES
	fi
	
	wait
	
}                 
    
	                                                 
generateCustom()
# Old-fashioned way of regenerating the build system from scratch : 
{


	echo "--- generating build system"
	
	if [ "$do_remove_generated" -eq 0 ] ; then
		echo
		echo " - removing all generated files"
		./cleanGeneratedConfigFiles.sh
	fi
	
	# Update timestamps since SVN may mess them up :
	CONFIG_SOURCE=configure-template.ac
	CONFIG_TARGET=configure.ac
	
	touch $CONFIG_SOURCE
	
		
	echo
	echo " - generating $CONFIG_TARGET, by filling $CONFIG_SOURCE with ${CEYLAN_SETTINGS_FILE}"

	# Generates 'configure.ac' with an already cooked dedicated Makefile :
	make -f MakeConfigure clean config-files SETTINGS_FILE=${CEYLAN_SETTINGS_FILE} SUBSTITUTE=${CEYLAN_SUBSTITUTE_SCRIPT}

	echo
	echo " - preparing libtool, by executing libtoolize"
	
	
	(libtool --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: You must have \`libtool' installed."
		echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/" 
		exit 20
   	}
	
	(libtoolize --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: You must have \`libtoolize' installed."
		echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/" 
		exit 21
   	}

	if test -z "$verbose"; then
		libtoolize_verbose=""
	else
		libtoolize_verbose="--debug"
	fi
	
	execute libtoolize --automake $copy $force $libtoolize_verbose
	
	echo
	echo " - generating aclocal.m4, by scanning configure.ac"
	
	(aclocal --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: Missing \`aclocal'.  The version of \`automake'"
		echo "installed does not appear recent enough."
		echo "You can get automake from ftp://ftp.gnu.org/pub/gnu/"
		exit 22
	}

	M4_DIR=. 
	
	ACLOCAL_OUTPUT=aclocal.m4
	
	# Do not use '--acdir=.' since it prevents aclocal from writing its file :
	execute aclocal -I $M4_DIR --output=$ACLOCAL_OUTPUT $force $verbose
	
	echo
	echo " - generating '.in' files from '.am' files with automake"

	(automake --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: You must have \`automake' installed."
		echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/"
		exit 24
	}

	automake_strictness="--foreign" 
	#automake_strictness="--gnu"
	
	execute automake --add-missing --include-deps $automake_strictness $warnings $copy $verbose  

	
	(autoconf --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: You must have \`autoconf' installed."
		echo "Download the appropriate package for your distribution,"
		echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
		exit 25
	}
	
	echo
	echo " - generating 'configure' script"
 	execute autoconf $warnings $force $verbose

	# Add GNU gettext (autopoint) ?
	
	if [ "$do_stop_after_configure" -eq 0 ] ; then
		echo
		echo "Now you are ready to run configure"
		return
	fi
		
	echo
	echo " - executing 'configure' script"
	
	(./configure --version) < /dev/null > /dev/null 2>&1 || {
		echo
		echo "**Error**: the 'configure' cannot be properly used"
		exit 26
	}
	

	if [ -n "$library_location_opt" ] ; then
		echo "(updating, for this script only, library search path with ${library_location}/lib)"
		LD_LIBRARY_PATH=$library_location/lib:$LD_LIBRARY_PATH
	fi
	
 	execute ./configure $configure_opt
	

	if [ "$do_clean" -eq 0 ] ; then
		echo
		echo " - cleaning all"
	 	execute make clean
	fi
	
	
	if [ "$do_build" -eq 0 ] ; then
		echo
		echo " - building all"
	 	execute make
	fi
	
	
	if [ "$do_install" -eq 0 ] ; then
		echo
		echo " - installing"
	 	execute make install
	else
	
		if [ -n "$library_location_opt" ] ; then
			echo 1>&2
			echo "Warning : not installing tests and using $library_location_opt implies updating library search paths to select the correct library, for example one may enter : " 1>&2
			echo "export LD_LIBRARY_PATH=$library_location/lib:\$LD_LIBRARY_PATH" 1>&2
		fi
		
	fi

	if [ "$do_test" -eq 0 ] ; then
		export LD_LIBRARY_PATH=$library_location/lib:$LD_LIBRARY_PATH
		echo
		echo " - running unit tests"
	 	execute make check
	fi
	
		
}


regenerateWithAutoreconf()
# The current way of having a build system up and running, since autoreconf
# has been fixed now. However it seems to
{

	echo "--- updating build system using autoreconf"
	
	autoreconf_opt="--force --install"
	autoreconf_warnings="--warnings=all" 
	
	autoreconf $autoreconf_opt $autoreconf_warnings
	
}
	
	
generateCustom
#regenerateWithAutoreconf



