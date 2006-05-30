# There is not need for the following two lines since this script is expected only 
# to be sourced (not executed) :

#!/bin/sh
#USAGE=". $0"

# This script is made so that, when sourced (not executed), main UNIX tools
# are defined with standard locations (i.e. find is to be found in /usr/bin/find
# or nowhere, etc.)


# Triggers termUtils.sh as well :
# Note : defaultLocations.sh depends on platformDetection.sh, not the contrary.

PLATFORMDETECT="platformDetection.sh"

if [ ! -f "${SHELLS_LOCATION}/${PLATFORMDETECT}" ] ; then
	if [ ! -f "./${PLATFORMDETECT}" ] ; then
		echo 1>&2
		echo "    Error, helper script for platform detection not found (${PLATFORMDETECT})." 1>&2
		exit 1
	else
		. ./${PLATFORMDETECT}
	fi
else
	. "${SHELLS_LOCATION}/${PLATFORMDETECT}"
fi


# Default locations for real basic commands:

CP="/bin/cp"
MV="/bin/mv"
LN="/bin/ln"
RM="/bin/rm"
MKDIR="/bin/mkdir"
FIND="/usr/bin/find"
DU="/usr/bin/du"


# For ping tool options :
if  [ "$is_windows" -eq 0 ] ; then
	PING_OPT="-n"
else
	PING_OPT="-c"
fi


# For MD5 sum tool :

if  [ "$is_bsd" -eq 1 ] ; then

	# Here we are not under a BSD-style OS :
	if findTool md5sum ; then
		MD5SUM=$returnedString
	else
		ERROR "No md5sum tool found, it is necessary in order to check file integrity."	
	fi
	
else
	
	# BSD does not use the same name :
	if findTool cksum ; then
		MD5SUM="$returnedString -5"
	else
		ERROR "No md5sum tool found (cksum), it is necessary in order to check file integrity."	
	fi
	
fi




# This flag indicates whether this script should warn if a basic tool is not
# found in its standard (usual) location [default : true (0)]. 
do_warn="0"


# This flag indicates whether this script should ensure that only registered 
# executables are used [default : true (0)]. 
be_strict="0"

# lookUpExec and findTool are to be found in platformDetection.sh



convertToWinPath()
# Converts a specified absolute path into a valid Windows one thanks to a provided prefix,
# and according to windows_prefix.
# Usage : convertToWinPath <a non-converted path> <a converted prefix>
# Ex : convertToWinPath /home/sye/Projects/OSDL/OSDL-0.3/src installs\\cygwin returns 
# in returnedString, with default settings : c:\\cygwin\\aaa\\bbb\\ccc
{

	if [ -z "$1" ] ; then
		ERROR "No path specified for convertToWinPath."
		exit 2
	fi

	if [ -z "$2" ] ; then
		ERROR "No prefix specified for convertToWinPath."
		exit 2
	fi
	
	returnedString=`${ECHO} "$1" | ${SED} 's|/|\\\\|g'`
	
	# No escaping for already converted prefix :
	returnedString="$2${returnedString}"
}

	


findBasicShellTools()
# Will automatically look up main basic UNIX tools.
# Uses implicitly be_strict to step on errors or not.
# Usage : findBasicShellTools
{

	findTool echo
	ECHO=$returnedString
	
	findTool cp
	CP=$returnedString

	findTool mv
	MV=$returnedString
	
	findTool ln
	LN=$returnedString
	
	findTool rm
	RM=$returnedString
		
	findTool mkdir
	MKDIR=$returnedString

	# In some cases already found by 'platformDetection.sh' :
	findTool grep
	GREP=$returnedString
	
	findTool find
	FIND=$returnedString
	
	findTool du
	DU=$returnedString
	
	findTool df
	DF=$returnedString
	
	# Usually already found by 'platformDetection.sh' :
	findTool uname
	UNAME=$returnedString
	
	findTool id
	ID=$returnedString
	
	findTool ps
	PS=$returnedString
	
	findTool wc
	WC=$returnedString
	
	findTool tail
	TAIL=$returnedString
	
	findTool sed
	SED=$returnedString
	
	findTool chmod
	CHMOD=$returnedString
	
	findTool cat
	CAT=$returnedString
	
	findTool which
	WHICH=$returnedString
	
	findTool tr
	TR=$returnedString	
	
	DEBUG "ECHO = $ECHO, CP = $CP, MV = $MV, LN = $LN, RM = $RM, MKDIR = $MKDIR, GREP = $GREP, FIND = $FIND, DU = $DU, DF = $DF, UNAME = $UNAME, ID = $ID, PS = $PS, WC = $WC, TAIL = $TAIL, SED = $SED, CHMOD = $CHMOD, CAT = $CAT, WHICH = $WHICH, TR = $TR"
	
}



findSupplementaryShellTools()
# Will automatically look up less basic UNIX tools.
# Uses implicitly be_strict to step on errors or not.
# Usage : findBinaryTools
{

	findTool awk
	AWK=$returnedString

	findTool tar
	TAR=$returnedString
	
	findTool gunzip
	GUNZIP=$returnedString
	
	findTool bunzip2
	BUNZIP2=$returnedString
	
	findTool ping
	PING=$returnedString
	
	findTool cvs
	CVS=$returnedString
		
	findTool sleep
	SLEEP=$returnedString
	
	DEBUG "AWK = $AWK, TAR = $TAR, GUNZIP = $GUNZIP, BUNZIP2 = $BUNZIP2, UNZIP = $UNZIP, PING = $PING, CVS = $CVS, MORE = $MORE, SLEEP = $SLEEP"
	
}



findBuildTools()
# Will automatically look up build tools.
# Uses implicitly be_strict to step on errors or not.
# Usage : findBuildTools
{

	if [ "$is_bsd" -eq 0 ] ; then
		findTool gmake
		MAKE=$returnedString
	else
		findTool make
		MAKE=$returnedString
	fi
	
	if ! `${MAKE} -v | grep GNU 1>/dev/null 2>&1` ; then		
		if [ $be_strict -eq 0 ] ; then
				ERROR "Tool look-up for make failed : this make does not seem to be GNU make."
				exit 1
			else
				WARNING "This make tool does not seem to be GNU make."
			fi
	else
		DEBUG "make GNU detected."		
	fi
	
	
	# Selecting C and C++ compilers.
	
	# Special case for minGW :

	if  [ "$use_mingw" -eq 0 ] ; then

		# MINGW_ROOT has been set by 'platformDetection.sh' :
	
		MINGW_PATH=${MINGW_ROOT}/bin
		export MINGW_PATH

		MINGW_LD_LIBRARY_PATH=${MINGW_ROOT}/lib
		export MINGW_LD_LIBRARY_PATH
            
		MINGW_CFLAGS="-I${MINGW_ROOT}/include"
		export MINGW_CFLAGS

		MINGW_LFLAGS="-L${MINGW_ROOT}/lib"
		export MINGW_LFLAGS
            
		MINGW_GCC=${MINGW_PATH}/gcc.exe
		export MINGW_GCC

		MINGW_GPP=${MINGW_PATH}/g++.exe
		export MINGW_GPP
         
		C_COMPILER=${MINGW_GCC}
		export C_COMPILER

		CPP_COMPILER=${MINGW_GPP}
		export CPP_COMPILER

		if [ ! -x "${C_COMPILER}" ] ; then
			ERROR "MinGW C compiler not available ($C_COMPILER)."
			exit 10
		fi

		if [ ! -x "${CPP_COMPILER}" ] ; then
			ERROR "MinGW C++ compiler not available ($CPP_COMPILER)."
			exit 11
		fi

		return 
		
	fi
	
	
	# Look for a C compiler, not a C++ compiler (for example, gcc, not g++).
	
	if [ -x "${C_COMPILER}" ] ; then
		DEBUG "C compiler taken from the C_COMPILER environment variable : ${C_COMPILER}."
	else
		findTool gcc
		GCC=$returnedString
		if [ ! -x ${GCC} ] ; then
			DEBUG "C compiler not found, defaulting to CC environment variable."
		else
			DEBUG "C compiler found in ${GCC}."
			C_COMPILER=${GCC}
			GCC_BASE=`dirname ${C_COMPILER}`
			GCC_ROOT=`dirname ${GCC_BASE}`
		fi
	fi
	
		
	# Look for a C++ compiler, not a C compiler (for example, g++, not gcc).

	if [ -x "${CPP_COMPILER}" ] ; then
		DEBUG "C++ compiler taken from the CPP_COMPILER environment variable : ${CPP_COMPILER}."
	else
		findTool g++
		GPP=$returnedString
		if [ ! -x ${GPP} ] ; then
			DEBUG "C++ compiler not found, defaulting to CC environment variable."
		else
			DEBUG "C++ compiler found in ${GPP}."
			CPP_COMPILER=${GPP}
			GCC_BASE=`dirname ${CPP_COMPILER}`
			GCC_ROOT=`dirname ${GCC_BASE}`

		fi
	fi
	
		
	if [ "$be_strict" -eq 0 ] ; then
	
		if [ ! -x "$C_COMPILER" ] ; then
			ERROR "No C compiler found, please update C_COMPILER environment variable and/or PATH."
			exit 2
		fi
		
		if [ ! -x "$CPP_COMPILER" ] ; then
			ERROR "No C++ compiler found, please update CPP_COMPILER environment variable and/or PATH."
			exit 3
		fi
		
	fi
	
	# Useful to have make tool use our compiler choices :

	unset BUILD_LOCATIONS
	
	if [ -n "$COMPILER_FAMILY" ] ; then
		BUILD_LOCATIONS="COMPILER_FAMILY=${COMPILER_FAMILY} ${BUILD_LOCATIONS}"
	fi	
	
	if [ -n "$GCC_ROOT" ] ; then
		BUILD_LOCATIONS="GCC_ROOT=$GCC_ROOT ${BUILD_LOCATIONS}"
	fi	
	
	if [ -x "$C_COMPILER" ] ; then
		BUILD_LOCATIONS="CC=$C_COMPILER ${BUILD_LOCATIONS}"
	fi	
	
	if [ -x "$CPP_COMPILER" ] ; then
		BUILD_LOCATIONS="CXX=$CPP_COMPILER ${BUILD_LOCATIONS}"
	fi		
	
	# flex, bison and perl currently disabled since not used :
			
	#findTool flex
	#FLEX=$returnedString
	
	#findTool bison
	#BISON=$returnedString
	
	#findTool perl
	#PERL=$returnedString
	
	DEBUG "MAKE = $MAKE, GCC = $GCC, CC = $CC, FLEX = $FLEX, BISON = $BISON, PERL = $PERL"
	
}


findMoreSpecificTools()
# Will automatically look up tools that are specific enough not to be available by default on
# some platforms. Requesting them from a portable script is therefore unsafe.
# Uses implicitly be_strict to step on errors or not.
# Usage : findBuildTools
{

	# Not available under NetBSD default installs : 
	findTool unzip
	UNZIP=$returnedString

	findTool more
	MORE=$returnedString

}


setBuildEnv()
# Sets the environment so that the build goes smooth by selecting the correct files
# Usage : setBuildEnv [--exportEnv] [command]
# Exemple : setBuildEnv --exportEnv ./configure --prefix=${prefix}/binutils-${binutils_VERSION}
{

	export_env=1
        
	if [ "$1" = "--exportEnv" ] ; then  
		DEBUG "Exporting main environment variables."
		export_env=0
		OLD_CC=$CC
		OLD_CXX=$CXX
		OLD_PATH=$PATH
		OLD_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
		shift
	fi
        
	command=$1
	shift
	command_string=" Command is : <$command $@>."
	
	current_path=$PATH
	current_ld_library_path=$LD_LIBRARY_PATH
	
	# So that tools ignoring CC and CXX have a chance to catch them nevertheless :
	if [ -d "${GCC_ROOT}" ] ; then
		current_path=${GCC_ROOT}/bin:$current_path
		current_ld_library_path=${GCC_ROOT}/lib:$current_ld_library_path
	fi
	
	if [ "$use_mingw" -eq 0 ] ; then
		current_path=${MINGW_PATH}:$current_path
		current_ld_library_path=${MINGW_LD_LIBRARY_PATH}:$current_ld_library_path
	fi

	DEBUG "Command path will be : <$current_path>."
	DEBUG "Command library path will be : <$current_ld_library_path>."

	if [ "export_env" -eq 0 ] ; then     
	   
		PATH="${current_path}"
		export PATH
		
		LD_LIBRARY_PATH="${current_ld_library_path}"
		export LD_LIBRARY_PATH
		
	fi
	
	DEBUG "C_COMPILER is ${C_COMPILER}."
	DEBUG "CPP_COMPILER is ${CPP_COMPILER}."
        
	if [ -x "${C_COMPILER}" ] ; then
        
		if [ "export_env" -eq 0 ] ; then        
			CC=${C_COMPILER}
			export CC
		fi
                
		if [ -x "${CPP_COMPILER}" ] ; then
		
			DEBUG "C and C++ compilers available. $command_string"			
                        
			if [ "export_env" -eq 0 ] ; then        
				CXX=${CPP_COMPILER}
				export CXX
			fi
                        
			if [ -n "$*" ] ; then
                        
				DEBUG "Actual command will be : " PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CC="${C_COMPILER}" CXX="${CPP_COMPILER}" $command "$@"
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CC="${C_COMPILER}" CXX="${CPP_COMPILER}" $command "$@"
				return $?
			else
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CC="${C_COMPILER}" CXX="${CPP_COMPILER}" $command 
				return $?				
			fi	
			
		else
			
			DEBUG "C compiler available, C++ not. $command_string"

			if [ -n "$*" ] ; then
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CC="${C_COMPILER}" $command "$@"
				return $?
			else
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CC="${C_COMPILER}" $command 
				return $?				
			fi	
					
		fi
	
	else
	
	
		if [ -x "${CPP_COMPILER}" ] ; then
	
			DEBUG "C++ compiler available, C not. $command_string"
                        
			if [ "export_env" -eq 0 ] ; then        
				CXX=${CPP_COMPILER}
				export CXX
			fi

			if [ -n "$*" ] ; then
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CXX="${CPP_COMPILER}" $command "$@"
				return $?
			else
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" CXX="${CPP_COMPILER}" $command 
				return $?				
			fi	
	
		else
	
			DEBUG "No C nor C++ compiler available. $command_string"
			
			if [ -n "$*" ] ; then
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" $command "$@"
				return $?
			else
				PATH="${current_path}" LD_LIBRARY_PATH="${current_ld_library_path}" $command 
				return $?				
			fi	

		fi
	
	fi
        
    if [ "export_env" -eq 0 ] ; then  
	
		CC=$OLD_CC
		export CC
		
		CXX=$OLD_CXX
		export CXX
		
		PATH=$OLD_PATH
		export PATH
		
		LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
		export LD_LIBRARY_PATH
		
	fi
       
}



# Auto-run on source to have basic commands :
findBasicShellTools
