#!/bin/sh


USAGE="Usage: "`basename $0`": cleans the install by removing all files generated for the build system. Useful while debugging the autotools."

FIND=`which find 2>/dev/null`
#echo "FIND = $FIND"

RM="/bin/rm"

# Must act from very first root directory.
# Ex: from Ceylan/trunk, not Ceylan/trunk/src
BASE_DIR=`dirname $0`/../../..
#echo "BASE_DIR = $BASE_DIR"

cd $BASE_DIR

# We are in trunk now.

# Converts a relative path into an absolute one:
BASE_DIR=`pwd`

#cd src/code/scripts/erlang
#make -s clean 1>/dev/null 2>&1


# Only the test Makefile (if available) knows which executables should be
# removed:
cd $BASE_DIR/test
make clean 1>/dev/null 2>&1

# Only to be run from the test directory, as the log plug example in the website
# would match as well:
$FIND . \( -name 'test*.exe-logs' -a -type d \) -exec $RM -rf '{}' ';' 2>/dev/null

cd cross-tests
make clean
cd ..


cd $BASE_DIR

$FIND . \( -name 'test*.exe' -a -type f \) -exec $RM -f '{}' ';' 2>/dev/null

# Directories:
$FIND . \( -name 'autom4te.cache' -o -name '.deps' -o -name '.libs' \) -exec $RM -rf '{}' ';' 2>/dev/null

# Files:
$FIND . \( -name ltmain.sh -o -name aclocal.m4 -o -name install-sh -o -name missing -o -name depcomp -o -name stamp-h1 -o -name configure.ac -o -name configure-from-autoscan.ac -o -name configure-from-autoupdate.ac -o -name configure -o -name 'auto*.log' -o -name 'auto*.err' -o -name config.guess  -o -name config.sub -o -name config.log -o -name config.status -o -name libtool -o -name Makefile -o -name Makefile.in -o -name CeylanTemporaryConfig.h -o -name CeylanConfig.h -o -name '*.so' -o -name '*.a' -o -name 'svn-commit*.tmp' -o -name '*.o' -o -name '*.loT' -o -name '*.lo' -o -name 'test*.log' -o -name 'test*.xml' -o -name '*.la' -o -name core \) -exec $RM -f '{}' ';' 2>/dev/null

$RM -f TODO MAINTENERS AUTHORS INSTALL FAQ ChangeLog THANKS NEWS COPYING.LIB README ceylan-*.tar.gz ceylan-*.tar.bz2 test/system/granularity.dat

$RM -f src/doc/basic/Ceylan-*-template.txt src/conf/doc/doxygen.conf src/conf/build/ceylan-*.pc src/conf/build/m4/ceylan.m4 src/code/CeylanConfig.h.in


# libltdl is generated by libtoolize in autogen.sh:
$RM -rf libltdl


echo "    Cleaning done."
