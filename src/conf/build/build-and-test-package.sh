#!/bin/bash


USAGE="`basename $0` : builds and installs the Ceylan library from a just-extracted distributed package, and builds, installs and runs the test suite against this install."

SUFFIX=`hostname`-`date '+%Y%m%d-%Hh%M'`
CEYLAN_INSTALL_ROOT=$HOME/tmp-ceylan-install-$SUFFIX
CEYLAN_INSTALL_TEST_ROOT=$HOME/tmp-ceylan-test-install-$SUFFIX

mkdir $CEYLAN_INSTALL_ROOT $CEYLAN_INSTALL_TEST_ROOT

CONFIGURE_COMMON_OPT=""
#CONFIGURE_COMMON_OPT="CXX=g++-3.4"

./configure ${CONFIGURE_COMMON_OPT} --prefix=$CEYLAN_INSTALL_ROOT && make && make install && cd test && ./configure ${CONFIGURE_COMMON_OPT} --prefix=$CEYLAN_INSTALL_TEST_ROOT --with-libCeylan=$CEYLAN_INSTALL_ROOT && make && make install && ./playTests.sh

