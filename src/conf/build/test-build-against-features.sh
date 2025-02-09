#!/bin/bash

USAGE="Usage :"`basename $0`" : builds Ceylan from scratch multiple times, using various feature sets, so thatmost variations are automatically checked."

COMMAND=$0

LAUNCH_DIR=`pwd`

# Always start from 'src/conf/build' directory :
cd `dirname $COMMAND`

AUTOGEN=autogen.sh

