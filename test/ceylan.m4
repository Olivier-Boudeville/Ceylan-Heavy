# Autoconf macro specifically written for Ceylan users.

# This file is therefore not generated, and should be kept as it is.

# This macro is of no use for the Ceylan library itself. It is provided to
# help user code, be it other libraries or programs, to have their 
# 'configure.ac' use correctly the Ceylan library.

# This m4 macro tests for the Ceylan users everything needed, including
# appropriate Ceylan headers and library. It then records the relevant 
# settings for the build of the user program or library.

# More precisely, this macro checks that the Ceylan header (Ceylan.h) is
# available, i.e. that it exists and can be preprocessed and compiled.
# Then a check is performed to ensure the Ceylan headers and the Ceylan 
# library that will be used match, i.e. that the library implements the API
# described in the headers (depending on the version, newer headers mixed
# with an obsolete library would fail at the link step).
# Finally, a test program is built and run to check that the full tool chain 
# behaves flawlessly, including with respect to the user-specified Ceylan
# version expected by the code being built after this configuration step
# (versions are compared according to the libtool rules about specified
# downward-compatibility).


# Ceylan users can execute 'aclocal' (ex : 'aclocal -I . --output=aclocal.m4)
# to have our macros added to their 'aclocal.m4', provided that 
# CEYLAN_PATH is called from the user configure.ac.

# This macro will take advantage of locations specified to the configure
# script, ex : ./configure --with-libCeylan=~/myPrefixedCeylanInstall

#
# CEYLAN_PATH( ac,rev,anc )
#
# Example : in configure.ac, CEYLAN_PATH( 0,3,0 )
#
AC_DEFUN([CEYLAN_PATH],
[
  CEYLAN_OLDEST_SUPPORTED_MAJOR=0
  CEYLAN_OLDEST_SUPPORTED_MINOR=3  
  # Setting the install path of the Ceylan library :
  CEYLAN_LIBS=" -lCeylan "
  AC_ARG_WITH(libCeylan,
    AS_HELP_STRING([--with-libCeylan],[path to the Ceylan library (prefix)]),
      [
        CEYLAN_CPPFLAGS="-I${withval}/include/Ceylan"
        CEYLAN_LIBS="-L${withval}/lib -lCeylan"
		LD_LIBRARY_PATH="${withval}/lib:$LD_LIBRARY_PATH"
		export LD_LIBRARY_PATH
      ],[
	  	# Use pkg-config if possible, otherwise choose default values :
		# PKG_CONFIG_PATH should contain /usr/local/lib/pkgconfig, use
		# export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
		# For the moment 
	  	PKG_CHECK_MODULES(CEYLAN, ceylan-$1.$2 >= $CEYLAN_OLDEST_SUPPORTED_MAJOR.$CEYLAN_OLDEST_SUPPORTED_MINOR,
			[
				CEYLAN_CPPFLAGS="${CEYLAN_CFLAGS} ${CEYLAN_CPPFLAGS}"
				LD_LIBRARY_PATH=`pkg-config ceylan-$1.$2 --variable=libdir`:$LD_LIBRARY_PATH
				export LD_LIBRARY_PATH
			],
			[
				default_install_path=/usr/local
				AC_MSG_WARN([pkg-config failed, defaulting to install path $default_install_path])
        		CEYLAN_CPPFLAGS="-I${default_install_path}/include/Ceylan"
        		CEYLAN_LIBS="-L${default_install_path}/lib -lCeylan"
			])	
		# If pkg-config is not available on your platform, you can just 
		# uncomment the following two lines (possibly updated if needed) :
		# CEYLAN_CPPFLAGS="-I/usr/local/include/Ceylan"
		# CEYLAN_LIBS="-L/usr/local/lib -lCeylan"
      ])
  # Add pthread flags in case the Ceylan multithreading feature is enabled :
  case "$target" in
      *-*-bsdi*)
    	  pthread_cflags="-D_REENTRANT -D_THREAD_SAFE"
    	  pthread_lib=""
    	  ;;
      *-*-darwin*)
    	  pthread_cflags="-D_THREAD_SAFE"
    	  # causes Carbon.p complaints?
    	  # pthread_cflags="-D_REENTRANT -D_THREAD_SAFE"
    	  ;;
      *-*-freebsd*)
    	  pthread_cflags="-D_REENTRANT -D_THREAD_SAFE"
    	  pthread_lib="-pthread"
    	  ;;
      *-*-netbsd*)
	  	  # -I/usr/pkg/include removed since its pthread.h had 
		  # conflicting types with /usr/include/pthread_types.h :
    	  pthread_cflags="-D_REENTRANT"
		  # -lsem removed, it does not exist with NetBSD 2.0.2 :
    	  pthread_lib="-L/usr/pkg/lib -lpthread"
		  # Needed for AC_RUN_IFELSE :
		  LD_LIBRARY_PATH="/usr/pkg/lib:$LD_LIBRARY_PATH"
		  export LD_LIBRARY_PATH
    	  ;;
      *-*-openbsd*)
    	  pthread_cflags="-D_REENTRANT"
    	  pthread_lib="-pthread"
    	  ;;
      *-*-solaris*)
    	  pthread_cflags="-D_REENTRANT"
    	  pthread_lib="-lpthread -lposix4"
    	  ;;
      *-*-sysv5*)
    	  pthread_cflags="-D_REENTRANT -Kthread"
    	  pthread_lib=""
    	  ;;
      *-*-irix*)
    	  pthread_cflags="-D_SGI_MP_SOURCE"
    	  pthread_lib="-lpthread"
    	  ;;
      *-*-aix*)
    	  pthread_cflags="-D_REENTRANT -mthreads"
    	  pthread_lib="-lpthread"
    	  ;;
      *-*-hpux11*)
    	  pthread_cflags="-D_REENTRANT"
    	  pthread_lib="-L/usr/lib -lpthread"
    	  ;;
      *-*-qnx*)
    	  pthread_cflags=""
    	  pthread_lib=""
    	  ;;
      *-*-osf*)
    	  pthread_cflags="-D_REENTRANT"
    	  if test x$ac_cv_prog_gcc = xyes; then
    		  pthread_lib="-lpthread -lrt"
    	  else
    		  pthread_lib="-lpthread -lexc -lrt"
    	  fi
    	  ;;
      *)
    	  pthread_cflags="-D_REENTRANT"
    	  pthread_lib="-lpthread"
    	  ;;
  esac
	  
  # Updating the overall build flags :
  CPPFLAGS="$CPPFLAGS $CEYLAN_CPPFLAGS $pthread_cflags"
  LIBS="$LIBS $CEYLAN_LIBS $pthread_lib"
  #AC_MSG_NOTICE([CPPFLAGS = $CPPFLAGS])
  #AC_MSG_NOTICE([LIBS = $LIBS])  
  AC_SUBST(CPPFLAGS)
  AC_SUBST(LIBS)
  # Checking for Ceylan header files :
  have_ceylan=yes
  AC_MSG_NOTICE([checking for the Ceylan library])
  # AC_CHECK_HEADERS checks better than AC_PREPROC_IFELSE :
  # (compilable more interesting than only preprocessable)
  AC_LANG_PUSH([C++]) 
  AC_CHECK_HEADERS([Ceylan.h],[],[have_ceylan=no])
  if test x$have_ceylan = xno; then
    AC_MSG_ERROR([No usable Ceylan header file (Ceylan.h) found. If the Ceylan library has been installed in a non-standard location (ex : when configuring it, --prefix=$HOME/myCeylanInstall was added), use the --with-libCeylan option, ex : --with-libCeylan=$HOME/myCeylanInstall.])
  fi
  # Now we must check that we will link indeed to the expected version
  # of Ceylan library, as specified by CEYLAN_PATH arguments :
  # Note : 
  #   - this test will be disabled if cross-compiling
  #   - we do not use AC_CACHE_CHECK and al on purpose, to avoid the
  # traps of cached entries.
  have_working_ceylan_library=yes
  AC_RUN_IFELSE([
  	AC_LANG_PROGRAM(
	  [[#include "Ceylan.h"]],[[Ceylan::Uint32 x;]])],
	[],[have_working_ceylan_library=no],[AC_MSG_WARN([No Ceylan library sanity test run, since cross-compiling])])
  AC_MSG_CHECKING([whether the Ceylan library can be linked to])
  if test x$have_working_ceylan_library = xyes; then
  	# Cross-compilation goes here, too.
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([The Ceylan library could not be successfully linked to.])  
  fi
  have_ceylan_headers_matching_library_version=yes
  AC_RUN_IFELSE([
  	AC_LANG_PROGRAM(
	  [[#include "Ceylan.h"]],[[CHECK_CEYLAN_VERSIONS();]])],
	[],[have_ceylan_headers_matching_library_version=no],[AC_MSG_WARN([No Ceylan library and headers matching test run, since cross-compiling])])
  AC_MSG_CHECKING([whether the Ceylan headers match the library version])
  if test x$have_ceylan_headers_matching_library_version = xyes; then
  	# Cross-compilation goes here, too.
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([The Ceylan installation does not seem to be clean, since headers did not match the library version.])  
  fi
  have_compatible_ceylan_version=yes
  AC_RUN_IFELSE([
  	AC_LANG_PROGRAM(
	  [[#include "Ceylan.h"]],[[Ceylan::LibtoolVersion targetVersion( "$1.$2.$3" ) ; if ( ! Ceylan::GetVersion().isCompatibleWith( targetVersion ) ) return 1 ;]])],
	[],[have_compatible_ceylan_version=no],[AC_MSG_WARN([No Ceylan version compatibility test run, since cross-compiling])])
  AC_MSG_CHECKING([whether we are linked to a $1.$2.$3-compatible Ceylan version])
  if test x$have_compatible_ceylan_version = xyes; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([The Ceylan library we would link to is not compatible with the one expected by the program being configured])  
  fi
  AC_LANG_POP([C++]) 
])
