#!/bin/sh

REPORT_FILE=`date '+%Y%m%d'`-`hostname`-Ceylan-bug-report.txt
BUG_ML_ADDRESS="ceylan-bugs@lists.sourceforge.net"
MESSAGE_PREFIX="###########################"
OFFSET="  + "

add()
{
	echo "${OFFSET}$*" >> ${REPORT_FILE}
}

addDir()
{
	if [ -d "$1" ] ; then
		add "$1 found, content is "
		add "<---"
		ls $1 2>&1 1>>${REPORT_FILE}
		add "end of directory listing for $1 --->"
	else
		add "No $1 found."
	fi
}


echo 
echo "        Welcome to Ceylan bug report generator."

echo
echo "This script has collected for you informations about your host configuration and your Ceylan installation, in order to ease the troubleshooting. The result of the inquiry has been stored in file <${REPORT_FILE}>, please feel free to peer into it before sending it."

# Blanks too any previous report file :
echo "${MESSAGE_PREFIX} Beginning of bug report" > ${REPORT_FILE}
echo >> ${REPORT_FILE}

add "Bug report generated for ${USER}@`hostname`, on `date '+%A %d %B, %Y at %H:%M:%S'`, from `pwd`" >> ${REPORT_FILE}

SETTINGS_FILE="CeylanSettings.inc"

if [ -f "${SETTINGS_FILE}" ] ; then
	add "${SETTINGS_FILE} found."
	add `grep MAJOR_VERSION= ${SETTINGS_FILE} |grep -v \#`	
	add `grep MINOR_VERSION= ${SETTINGS_FILE} |grep -v \#`	
	add `grep RELEASE= ${SETTINGS_FILE} |grep -v \#`	
else
	add "No ${SETTINGS_FILE} found."
fi

  
add "Host platform : `uname -a`"
add "Available disk size : " `df -k .`
 

add "gcc : `gcc -v 2>&1`"
add "ld : `ld -v 2>&1`"
add "PATH : $PATH"
add "LD_LIBRARY_PATH : $LD_LIBRARY_PATH "

echo >> ${REPORT_FILE} 
echo >> ${REPORT_FILE} 
echo "${MESSAGE_PREFIX} End of bug report" >> ${REPORT_FILE} 

echo
echo "You should send this report (${REPORT_FILE}) as attachment to our mail account dedicated to troubleshooting, ${BUG_ML_ADDRESS} (no registering needed), we will do our best to help you !" 


