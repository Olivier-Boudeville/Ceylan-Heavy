# Bash configuration file.

# Created 2002, June 26.
# Author: Olivier Boudeville (olivier.boudeville@online.fr)

# This script triggers the UNIX-common part (.bashrc.common) and then the
# OS-specific one, if available (ex: .bashrc.Linux, .bashrc.SunOS, etc.)
#
# An initial host-related overriding hook is provided as well.


# Retrieves the directory where this file and its helper files are stored:
MYHOME=$HOME


# Per-host configuration, if needed:
if [ -f $MYHOME/.bashrc.local ] ; then
	source $MYHOME/.bashrc.local
fi


# Common to all Unices:
if [ -f $MYHOME/.bashrc.common ] ; then
	source $MYHOME/.bashrc.common
fi


# Particular to this specific operating system:
if [ -f $MYHOME/.bashrc.`uname` ] ; then
	source $MYHOME/.bashrc.`uname`
fi
