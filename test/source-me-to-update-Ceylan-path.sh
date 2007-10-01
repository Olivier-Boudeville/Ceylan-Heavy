# This script is meant to be sourced, so that from a Cygwin shell
# the tests can be launched locally (where they are built) but
# dynamically linked with the appropriate Ceylan library.
# This applies to LOANI-based Ceylan builds.

#Usage : source update-test-env.sh

if [ ! -f "playTests.sh" ] ; then
  echo "Error, this script must be sourced from Ceylan/trunk/test, nothing done." 1>&2
else
  LIBS_LOCATION=`cygpath -a .`../../../../../LOANI-installations/OSDL-libraries/debug-mt/dll
  #echo $LIBS_LOCATION
  #ls "$LIBS_LOCATION"
  export PATH=$LIBS_LOCATION:$PATH
fi

# For most UNIX platforms :
LD_LIBRARY_PATH=`pwd`/../src/code:$LD_LIBRARY_PATH

#echo $PATH

