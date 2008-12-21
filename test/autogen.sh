#!/bin/sh


USAGE="
Usage: "`basename $0`" [ -g | --guess-ceylan-prefix ] [ -n | --no-build ] [ -o | --only-prepare-dist] [--ceylan-install-prefix <a prefix>]: (re)generates all the autotools-based build system for Ceylan tests.

	--guess-ceylan-prefix: try to guess where a supposedly corresponding prefixed Ceylan install lies. If one is found, then it is used, otherwise stops on failure
	--no-build: stop just after having generated the configure script
	--only-prepare-dist: perform only necessary operations so that the test directory can be distributed afterwards
	--ceylan-install-prefix <a prefix>: use the specified prefix to find the Ceylan library installation. Example: --ceylan-install-prefix $HOME/tmp-Ceylan-test-install"

# These tests must rely on a related Ceylan source directory, since they:
#	- need to know which Ceylan version is to be tested
#	- use some Ceylan facilities (ex: Ceylan substitute script)


# Main settings section.

# 0 means true, 1 means false:
do_remove_generated=0
do_stop_after_configure=1
do_clean=0
do_build=0
do_install=0
do_test=0

# Default install prefix:
ceylan_install_prefix="/usr/local"




while [ $# -gt 0 ] ; do
	token_eaten=1
		
	if [ "$1" = "-g" -o "$1" = "--guess-ceylan-prefix" ] ; then
	
		# Here we suppose we want to find a LOANI-installed Ceylan:
		COMMAND=$0

		# Always start from the 'trunk/test' directory:
		cd `dirname $COMMAND`

		# Let's guess this Ceylan version:
		ceylan_version=`grep CEYLAN_VERSION ../src/code/CeylanConfig.h|awk '{printf $3}'|sed 's|^.||1'|sed 's|.$||1'`
		#echo "guessed ceylan_version = $ceylan_version"	


		# Default value guessed from current path:
		loani_repository=`pwd|sed 's|/ceylan/Ceylan/trunk/test||1'`
		#echo "loani_repository = $loani_repository"

		loani_installations=`dirname $loani_repository`/LOANI-installations
		#echo "loani_installations = $loani_installations"

		ceylan_install_prefix="${loani_installations}/Ceylan-${ceylan_version}"
		
		if [ ! -d "$ceylan_install_prefix" ] ; then
			echo "Error, guessed prefix for Ceylan install ($ceylan_install_prefix) does not exist.$USAGE" 1>&2
			exit 10
		fi

		# One more chance to rely on a proper libtool:
		. ${loani_installations}/OSDL-environment.sh

		token_eaten=0
	fi

	if [ "$1" = "-n" -o "$1" = "--no-build" ] ; then
		do_stop_after_configure=0
		token_eaten=0
	fi

	if [ "$1" = "-o" -o "$1" = "--only-prepare-dist" ] ; then
		do_stop_after_configure=0
		token_eaten=0
	fi
	
	if [ "$1" = "--ceylan-install-prefix" ] ; then
		shift
		ceylan_install_prefix="$1"
		if [ ! -d "$ceylan_install_prefix" ] ; then
			echo "Error, specified prefix for Ceylan install ($ceylan_install_prefix) does not exist.$USAGE" 1>&2
			exit 11
		fi
		token_eaten=0
	fi
	
	if [ "$1" = "-h" -o "$1" = "--help" ] ; then
		echo "$USAGE"
		exit
		token_eaten=0
	fi

	if [ $token_eaten -eq 1 ] ; then
		echo "Error, unknown argument ($1).$USAGE" 1>&2
		exit 4
	fi	
	shift
done


# debug mode activated iff equal to true (0):
debug_mode=1

debug()
{
	if [ $debug_mode -eq 0 ] ; then
		echo "debug: $*"
	fi	
}


# Where the Ceylan library should be found:

if [ -n "$ceylan_install_prefix" ] ; then
	ceylan_install_prefix_opt="--with-ceylan-prefix=$ceylan_install_prefix"
fi


# Where these tests should be installed:
test_install_location="$ceylan_install_prefix"

if [ -n "$test_install_location" ] ; then
	test_install_location_opt="--prefix=$test_install_location"
else
	test_install_location_opt=""
fi


# To check the user can override them:
#test_overriden_options="CPPFLAGS=\"-DTEST_CPPFLAGS\" LDFLAGS=\"-LTEST_LDFLAGS\""
test_overriden_options=""

configure_opt="-enable-strict-ansi --enable-debug --disable-rpath $ceylan_install_prefix_opt $test_install_location_opt $test_overriden_options"


RM="/bin/rm -f"




# Wait-after-execution mode activated iff equal to true (0):
wait_activated=1

wait()
{

	if [ $wait_activated -eq 0 ] ; then
		echo "  <press enter key to continue>"
		read
	fi	
	
}

cd `dirname $0`

# Searches for the Ceylan substitute script:
CEYLAN_SUBSTITUTE_SCRIPT="../src/code/scripts/shell/substitute.sh"
if [ ! -x "${CEYLAN_SUBSTITUTE_SCRIPT}" ] ; then
	CEYLAN_SUBSTITUTE_SCRIPT="../code/scripts/shell/substitute.sh"
	if [ ! -x "${CEYLAN_SUBSTITUTE_SCRIPT}" ] ; then
	echo "Error, no executable Ceylan substitute script found." 1>&2
	exit 1
	fi
fi


# Searches for the Ceylan settings file:
CEYLAN_SETTINGS_FILE="../src/conf/CeylanSettings.inc"
if [ ! -f "${CEYLAN_SETTINGS_FILE}" ] ; then
	echo "Error, no Ceylan settings file found (${CEYLAN_SETTINGS_FILE})." 1>&2
	exit 2
fi


# Log-on-file mode activated iff equal to true (0):
log_on_file=1

log_filename="autogen.log"

debug "log_filename = $log_filename"
if [ -f "$log_filename" ]; then
	$RM "$log_filename"
fi


# Overall autotools settings:

# Be verbose for debug purpose:
#verbose="--verbose"
verbose=""

# Copy files instead of using symbolic link:
copy="--copy"
#copy=""

# Replace existing files:
#force="--force"
force=""

# Warning selection: 
warnings="--warnings=all"
#warnings=""



echo
echo "Bootstrapping now build for test system thanks to the autotools"
echo "      (this may take a while ...)"
echo


execute()
{

	echo "    Executing $*"
	
	if [ $log_on_file -eq 0 ] ; then
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
		if [ $log_on_file -eq 0 ] ; then
			echo "Error while executing '$*', see $log_filename" 1>&2
		else
			echo "Error while executing '$*'" 1>&2
			if [ "$1" = "./configure" ]; then
				echo "
Note: check the following log:" test/config.log	
  			fi
			
			if [ "$1" = "aclocal" ]; then
				echo "
Hint: look at the --ceylan-install-prefix option, with a parameter that could be similar to $HOME/Projects/LOANI-x.y/LOANI-installations/Ceylan-p.q"
  			fi
			
		fi
			
		exit $RES
	fi
	
	wait
	
}                 
    
	                                                 
generateCustom()
# Old-fashioned way of regenerating the build system from scratch: 
{


	echo "--- generating build system"
	
	if [ "$do_remove_generated" -eq 0 ] ; then
		echo
		echo " - removing all generated files"
		./cleanGeneratedConfigFiles.sh
	fi
	
	# Update timestamps since SVN may mess them up:
	CONFIG_SOURCE=configure-template.ac
	CONFIG_TARGET=configure.ac
	
	touch $CONFIG_SOURCE
	
		
	echo
	echo " - generating $CONFIG_TARGET, by filling $CONFIG_SOURCE with ${CEYLAN_SETTINGS_FILE}"

	# Generates 'configure.ac' with an already cooked dedicated Makefile:
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

	# Where ceylan.m4, pkg.m4, etc. should be found: 
	CEYLAN_M4_DIR=$ceylan_install_prefix/share/Ceylan
	
	ACLOCAL_OUTPUT=aclocal.m4
	
	# With newer libtool (ex: 2.2.4), we need to include a whole bunch of *.m4
	# files, otherwise 'warning: LTOPTIONS_VERSION is m4_require'd but not
	# m4_defun'd' ... ', same thing for LTSUGAR_VERSION, LTVERSION_VERSION, etc.
	GUESSED_LIBTOOL_BASE=`which libtool|sed 's|/bin/libtool$||1'`

	# Do not use '--acdir=.' since it prevents aclocal from writing its file:
	execute aclocal -I $CEYLAN_M4_DIR -I ${GUESSED_LIBTOOL_BASE}/share/aclocal --output=$ACLOCAL_OUTPUT $force $verbose
	
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
	

	if [ -n "$ceylan_install_prefix_opt" ] ; then
		echo "(updating, for this script only, library search path with ${ceylan_install_prefix}/lib)"
		LD_LIBRARY_PATH=$ceylan_install_prefix/lib:$LD_LIBRARY_PATH
	fi
	
 	execute ./configure $configure_opt
	

	if [ $do_clean -eq 0 ] ; then
		echo
		echo " - cleaning all"
	 	execute make clean
	fi
	
	
	if [ $do_build -eq 0 ] ; then
		echo
		echo " - building all"
	 	execute make
	fi
	
	
	if [ $do_install -eq 0 ] ; then
		echo
		echo " - installing"
	 	execute make install
	else
	
		if [ -n "$ceylan_install_prefix_opt" ] ; then
			echo 1>&2
			echo "Warning: not installing tests and using $ceylan_install_prefix_opt implies updating library search paths to select the correct library, for example one may enter: " 1>&2
			echo "export LD_LIBRARY_PATH=$ceylan_install_prefix/lib:\$LD_LIBRARY_PATH" 1>&2
		fi
		
	fi

	if [ $do_test -eq 0 ] ; then
		export LD_LIBRARY_PATH=$ceylan_install_prefix/lib:$LD_LIBRARY_PATH
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



