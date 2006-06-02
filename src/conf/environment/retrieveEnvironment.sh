#!/bin/bash



USAGE="Usage : "`basename $0`" [<install root>] [ -d | --debug ] [ --chekout | -c ] [ -h | --help ]\n Settles down the developer environment, from environment already described in <install root>, if specified, otherwise from environment defined in the source tree of this script. If --link is set, will not install anything but expect to be able to link on an existing environment already set in <install root>.\nExample : `basename $0` $HOME/Projects/ceylan --link"

# Other install roots could be : 
#   - /mnt/raid/md0/LOANI-0.3/LOANI-installations
#   - $HOME/Projects/OSDL-loanized/LOANI-installations

# Default settings.

SVN_PARAMETER="https://svn.sourceforge.net/svnroot/ceylan"

BACKUP_SUFFIX="previous"


# Debug mode (off by default, i.e. set to 1)
do_debug=1

# Link mode (on by default, i.e. set to 0)
do_link=0


DEBUG()
{
	[ "$do_debug" == 1 ] || echo "Debug : $*"
}

ERROR()
{
	echo "Error : $*"
	exit 1
}


# Assume -h or --help will never be install roots !
if [ "$1" == "-h" -o "$1" == "--help" ] ; then
	echo -e "$USAGE"
	exit 0
fi	


DEBUG "There are $# arguments."

INSTALL_ROOT=""

while [ $# -gt 0 ] ; do

	token_eaten=1
	
	DEBUG  "Evaluating argument $1."

	if [ "$1" == "-d" -o "$1" == "--debug" ] ; then
		DEBUG "Debug mode activated."
		do_debug=0
		token_eaten=0
	fi
	
	if [ "$1" == "-c" -o "$1" == "--checkout" ] ; then
		DEBUG "SVN checkout mode activated"
		do_link=1
		token_eaten=0		
	fi
	
	if [ "$1" == "-h" -o "$1" == "--help" ] ; then
		echo -e "Help requested : $USAGE"
		exit 0
	fi
	
	if [ "$token_eaten" == "1" ] ; then
		INSTALL_ROOT="$1"
	fi
	
	shift
	
done



if [ -z "$INSTALL_ROOT" ]; then
	INSTALL_ROOT=`dirname $0`/../../../../..
	echo "No install root specified, using <$INSTALL_ROOT>."
fi

if [ ! -d $INSTALL_ROOT ]; then
	echo "Error, install root directory <$INSTALL_ROOT> does not exist, create it first." 
	echo -e "$USAGE"
	exit 2
fi


retrieveProjects()
{
	
	DEBUG "It should be is LOANI's job. Consider using it."

	# This part, when linking is disabled, is seldom tested.
	
	echo "   - installing SVN-based projects into $INSTALL_ROOT"

	cd $INSTALL_ROOT

	CEYLAN_MODULES="ceylan"

	for m in $CEYLAN_MODULES; do
		echo "      + retrieving module $m"
		svn $SVN_PARAMETER co $m
		echo
		echo 
		echo
	done

}


checkBackupable()
# Checks that no back-up file is already existing, so that any new backup
# would not overwrite a previous one.
# Usage : checkBackupable <file>
{

	if [ -z "$1" ] ; then
		ERROR "No file to check for back-up specified."
		exit 10
	fi
	
	if [ -e "${1}.${BACKUP_SUFFIX}" -o -h "${1}.${BACKUP_SUFFIX}" ] ; then
		ERROR "A previous back-up of your file ${1} already exists (${1}.${BACKUP_SUFFIX}), probably because of a previous developer environment retrieval. Please check whether all these {.*|*}.${BACKUP_SUFFIX} files are to be kept, if not, remove them all first : this script will not overwrite any file."
		exit 11
	fi

}


backUpFile()
# Make, if possible, a back-up of specified file. Step on errors.
# Usage : backUpFile <file>
{
	if [ -z "$1" ] ; then
		ERROR "No file to back-up specified."
		exit 20
	fi
	checkBackupable ${1}
	mv -f ${1} ${1}.$BACKUP_SUFFIX
}


prepareDeveloperEnvironment()
{

	BASE=$INSTALL_ROOT/Ceylan/trunk/src/conf/environment
	
	
	echo "   - preparing developer environment"
	
	
	echo "      + configuring Xdefaults, vi, bash, CVS, and related"
		
	for f in $BASE/.Xdefaults $BASE/.vimrc $BASE/.bash* $BASE/.cvsrc; do
		if [ -e $HOME/`basename $f` -o -h $HOME/`basename $f` ] ; then
			backUpFile $HOME/`basename $f`
		fi	
		ln -s $f $HOME/`basename $f`
	done
	
	
	echo "      + configuring nedit"
	
	mkdir -p $HOME/.nedit
	if [ -f $HOME/.nedit/nedit.rc -o -h $HOME/.nedit/nedit.rc ] ; then
		backUpFile $HOME/.nedit/nedit.rc
	fi
	ln -s $BASE/nedit.rc $HOME/.nedit/nedit.rc
	
		
	echo "      + creating basic temporary directories"
	
	mkdir -p $HOME/tmp/tmp1
	mkdir -p $HOME/tmp/tmp2
	mkdir -p $HOME/tmp/tmp3
	mkdir -p $HOME/tmp/tmp4
	mkdir -p $HOME/tmp/tmp5

	
}


echo 
echo "Retrieving classical developer environment."

echo
echo "(if not done already, consider publishing your SSH public key to Sourceforge to avoid typing your password multiple times)" 
echo

if [ "$do_link" == "1" ] ; then
	retrieveProjects
fi

prepareDeveloperEnvironment

echo
if [ "$do_link" == "1" ] ; then
	echo "End of links creation. Maybe the LOANI_BASE variable in ~/.bashrc.local should be updated."
else
	echo "End of retrieval, projects files should be found in $INSTALL_ROOT."
fi
