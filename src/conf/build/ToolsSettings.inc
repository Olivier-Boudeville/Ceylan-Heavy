# Centralized version and release informations about tools being used for 
# Ceylan. 

# This file is intended to be both included in Makefiles and sourced 
# in shell (sh) scripts.




# ----------- For required tools :




# GNU Libtool : http://www.gnu.org/software/libtool/libtool.html

libtool_VERSION="1.5.20"
libtool_ARCHIVE="libtool-${libtool_VERSION}.tar.gz"
libtool_DOWNLOAD_LOCATION="ftp://ftp.cs.tu-berlin.de/pub/gnu/libtool"
libtool_ALTERNATE_DOWNLOAD_LOCATION="http://mirrors.usc.edu/pub/gnu/libtool"
libtool_MD5="fc3b564700aebcb19de841c1c2d66e99"




# win32 pthreads : http://sources.redhat.com/pthreads-win32/

# Use g++ with standard C++ exceptions :
win_pthread_DLL="pthreadGCE.dll"
win_pthread_DLL_MD5="c1a62e3a4d457e70258c6054a6cf5279"

win_pthread_VERSION="2003-09-18"
win_pthread_ARCHIVE="${win_pthread_DLL}"
win_pthread_wget_dir="sources.redhat.com"
win_pthread_ftp_dir="pthreads-dll-${win_pthread_VERSION}"
win_pthread_DOWNLOAD_LOCATION="ftp://${win_pthread_wget_dir}/pub/pthreads-win32/${win_pthread_ftp_dir}"
win_pthread_ALTERNATE_DOWNLOAD_LOCATION=""

win_pthread_HEADER_MAIN="pthread.h"
win_pthread_HEADER_MAIN_MD5="36f00accd29be66dd1784b8c084001cb"

win_pthread_HEADER_SCHED="sched.h"
win_pthread_HEADER_SCHED_MD5="f1969e2a9c55492097831db0f22e45b9"

win_pthread_HEADER_SEM="semaphore.h"
win_pthread_HEADER_SEM_MD5="aa0a28552311dde63fe62661d39d5a27"

# checking for whole directory will be always false, 
# actual test will be made in getwin_pthread. 
win_pthread_MD5="XX"




# Ceylan (libCeylan.so) : http://osdl.sourceforge.net/Ceylan-${Ceylan_VERSION}

Ceylan_VERSION="0.3"
Ceylan_RELEASE=0
Ceylan_ARCHIVE="Ceylan-${Ceylan_VERSION}.${Ceylan_RELEASE}.tar.bz2"
Ceylan_DOWNLOAD_LOCATION="http://osdn.dl.sourceforge.net/sourceforge/osdl"
Ceylan_ALTERNATE_DOWNLOAD_LOCATION="http://easynews.dl.sourceforge.net/sourceforge/osdl"
Ceylan_MD5="(blanked)"

Ceylan_ROOT="Ceylan/Ceylan-${Ceylan_VERSION}"
Ceylan_CVS_SERVER=cvs.sourceforge.net




# ----------- For common build tools :



# gcc : http://www.gnu.org/software/gcc/gcc.html

gcc_VERSION="3.4.4"

gcc_ARCHIVE="gcc-${gcc_VERSION}.tar.bz2"
gcc_DOWNLOAD_LOCATION="ftp://ftp.fu-berlin.de/unix/languages/gcc/releases/gcc-${gcc_VERSION}"
gcc_ALTERNATE_DOWNLOAD_LOCATION="ftp://ftp.uvsq.fr/pub/gcc/releases/gcc-${gcc_VERSION}"
gcc_MD5="b594ff4ea4fbef4ba9220887de713dfe"
gcc_BUILD_TREE="gcc-${gcc_VERSION}-buildTree"
gcc_SOURCE_TREE="gcc-${gcc_VERSION}-sourceTree"

# To test newer 4.0.1 gcc : 
# SDL 1.2.8 will not compile with it :
# "src/hermes/HeadMMX.h:53: error: 'ConvertMMX' aliased to undefined symbol '_ConvertMMX'"

#gcc_VERSION="4.0.1"
#gcc_ARCHIVE="gcc-${gcc_VERSION}.tar.bz2"
#gcc_DOWNLOAD_LOCATION="ftp://ftp.lip6.fr/pub/gcc/releases/gcc-${gcc_VERSION}"
#gcc_MD5="947416e825a877a0d69489be1be43be1"
#gcc_BUILD_TREE="gcc-${gcc_VERSION}-buildTree"
#gcc_SOURCE_TREE="gcc-${gcc_VERSION}-sourceTree"



# binutils : http://www.gnu.org/software/binutils/

binutils_VERSION="2.16.1"
binutils_ARCHIVE="binutils-${binutils_VERSION}.tar.gz"
binutils_DOWNLOAD_LOCATION="ftp://ftp.cs.tu-berlin.de/pub/gnu/binutils"
binutils_ALTERNATE_DOWNLOAD_LOCATION="http://ftp.gnu.org/gnu/binutils"
binutils_MD5="818bd33cc45bfe3d5b4b2ddf288ecdea"



# gdb : http://www.gnu.org/software/gdb/gdb.html

gdb_VERSION="6.3"
gdb_ARCHIVE="gdb-${gdb_VERSION}.tar.bz2"
gdb_DOWNLOAD_LOCATION="ftp://ftp.cs.tu-berlin.de/pub/gnu/gdb"
gdb_ALTERNATE_DOWNLOAD_LOCATION="http://ftp.gnu.org/gnu/gdb"
gdb_MD5="05b928f41fa5b482e49ca2c24762a0ae"





# ----------- For optional tools ;



# dot : http://www.research.att.com/sw/tools/graphviz/

dot_VERSION="2.6"
dot_ARCHIVE="graphviz-${dot_VERSION}.tar.gz"
dot_DOWNLOAD_LOCATION="http://www.graphviz.org/pub/graphviz/ARCHIVE"
dot_ALTERNATE_DOWNLOAD_LOCATION=""
dot_MD5="0d61fc4f8660be31503d4f9ab6f26bf0"



# doxygen : http://www.stack.nl/~dimitri/doxygen/

doxygen_VERSION="1.4.5"
doxygen_ARCHIVE="doxygen-${doxygen_VERSION}.src.tar.gz"
doxygen_DOWNLOAD_LOCATION="ftp://ftp.stack.nl/pub/users/dimitri"
doxygen_ALTERNATE_DOWNLOAD_LOCATION=""
doxygen_MD5="6319afd44c47a4bcf49658c995322f0e"



# tidy : http://tidy.sourceforge.net/ (currently deactivated)

tidy_ARCHIVE="tidy_src.tgz"
tidy_DOWNLOAD_LOCATION="http://tidy.sourceforge.net/src"
tidy_ALTERNATE_DOWNLOAD_LOCATION=""
# Changes often, not serious if does not match :
tidy_MD5="XXX"


# End of Ceylan tools definitions.
