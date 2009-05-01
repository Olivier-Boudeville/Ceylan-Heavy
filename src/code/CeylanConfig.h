#ifndef _SRC_CODE_CEYLANCONFIG_H
#define _SRC_CODE_CEYLANCONFIG_H 1
 
/* src/code/CeylanConfig.h. Generated automatically at end of configure. */
/* src/code/CeylanTemporaryConfig.h.  Generated from CeylanTemporaryConfig.h.in by configure.  */
/* src/code/CeylanTemporaryConfig.h.in.  Generated from configure.ac by autoheader.  */

/* Define to 1 if the target platform is a BSD one */
#ifndef CEYLAN_ARCH_BSD 
#define CEYLAN_ARCH_BSD  0 
#endif

/* Define to 1 if the target platform is FreeBSD */
#ifndef CEYLAN_ARCH_FREEBSD 
#define CEYLAN_ARCH_FREEBSD  0 
#endif

/* Define to 1 if the target platform is GNU/Linux */
#ifndef CEYLAN_ARCH_LINUX 
#define CEYLAN_ARCH_LINUX  1 
#endif

/* Define to 1 if the target platform is Mac OS X */
#ifndef CEYLAN_ARCH_MACOSX 
#define CEYLAN_ARCH_MACOSX  0 
#endif

/* Define to 1 if the target platform is NetBSD */
#ifndef CEYLAN_ARCH_NETBSD 
#define CEYLAN_ARCH_NETBSD  0 
#endif

/* Define to 1 if the target platform is the Nintendo DS */
#ifndef CEYLAN_ARCH_NINTENDO_DS 
#define CEYLAN_ARCH_NINTENDO_DS  0 
#endif

/* Define to 1 if the target platform is OpenBSD */
#ifndef CEYLAN_ARCH_OPENBSD 
#define CEYLAN_ARCH_OPENBSD  0 
#endif

/* Define to 1 if the target platform is Solaris */
#ifndef CEYLAN_ARCH_SOLARIS 
#define CEYLAN_ARCH_SOLARIS  0 
#endif

/* Define to 1 if the target platform is a UNIX one */
#ifndef CEYLAN_ARCH_UNIX 
#define CEYLAN_ARCH_UNIX  1 
#endif

/* Define to 1 if the target platform is a Windows one */
#ifndef CEYLAN_ARCH_WINDOWS 
#define CEYLAN_ARCH_WINDOWS  0 
#endif

/* Define to 1 if the `closedir' function returns void instead of `int'. */
/* #undef CEYLAN_CLOSEDIR_VOID */

/* Define to 1 if generic debug mode is to be enabled */
#ifndef CEYLAN_DEBUG 
#define CEYLAN_DEBUG  0 
#endif

/* Define to 1 if debug mode for console is to be enabled */
#ifndef CEYLAN_DEBUG_CONSOLE 
#define CEYLAN_DEBUG_CONSOLE  0 
#endif

/* Define to 1 if debug mode for name mangling is to be enabled */
#ifndef CEYLAN_DEBUG_DEMANGLE 
#define CEYLAN_DEBUG_DEMANGLE  0 
#endif

/* Define to 1 if debug mode for events is to be enabled */
#ifndef CEYLAN_DEBUG_EVENTS 
#define CEYLAN_DEBUG_EVENTS  0 
#endif

/* Define to 1 if debug mode for FIFO-based IPC is to be enabled */
#ifndef CEYLAN_DEBUG_FIFO 
#define CEYLAN_DEBUG_FIFO  0 
#endif

/* Define to 1 if debug mode for log system is to be enabled */
#ifndef CEYLAN_DEBUG_LOG 
#define CEYLAN_DEBUG_LOG  0 
#endif

/* Define to 1 if debug mode for low-level streams is to be enabled */
#ifndef CEYLAN_DEBUG_LOW_LEVEL_STREAMS 
#define CEYLAN_DEBUG_LOW_LEVEL_STREAMS  0 
#endif

/* Define to 1 if debug mode for marshallers is to be enabled */
#ifndef CEYLAN_DEBUG_MARSHALLERS 
#define CEYLAN_DEBUG_MARSHALLERS  0 
#endif

/* Define to 1 if debug mode for network clients is to be enabled */
#ifndef CEYLAN_DEBUG_NETWORK_CLIENTS 
#define CEYLAN_DEBUG_NETWORK_CLIENTS  0 
#endif

/* Define to 1 if debug mode for network servers is to be enabled */
#ifndef CEYLAN_DEBUG_NETWORK_SERVERS 
#define CEYLAN_DEBUG_NETWORK_SERVERS  0 
#endif

/* Define to 1 if debug mode for Nintendo DS is to be enabled */
#ifndef CEYLAN_DEBUG_NINTENDO_DS 
#define CEYLAN_DEBUG_NINTENDO_DS  0 
#endif

/* Define to 1 if debug mode for protocol servers is to be enabled */
#ifndef CEYLAN_DEBUG_PROTOCOL_SERVERS 
#define CEYLAN_DEBUG_PROTOCOL_SERVERS  0 
#endif

/* Define to 1 if debug mode for random generation is to be enabled */
#ifndef CEYLAN_DEBUG_RANDOM 
#define CEYLAN_DEBUG_RANDOM  0 
#endif

/* Define to 1 if debug mode for system layer is to be enabled */
#ifndef CEYLAN_DEBUG_SYSTEM 
#define CEYLAN_DEBUG_SYSTEM  0 
#endif

/* Define to 1 if debug mode for text buffer is to be enabled */
#ifndef CEYLAN_DEBUG_TEXTBUFFER 
#define CEYLAN_DEBUG_TEXTBUFFER  0 
#endif

/* Define to 1 if debug mode for threads is to be enabled */
#ifndef CEYLAN_DEBUG_THREADS 
#define CEYLAN_DEBUG_THREADS  0 
#endif

/* Define to 1 if debug mode for XML is to be enabled */
#ifndef CEYLAN_DEBUG_XML 
#define CEYLAN_DEBUG_XML  0 
#endif

/* Define to 1 if you have the `abs' function. */
#ifndef CEYLAN_USES_ABS 
#define CEYLAN_USES_ABS  1 
#endif

/* Define to 1 if you have the <arpa/inet.h> header file. */
#ifndef CEYLAN_USES_ARPA_INET_H 
#define CEYLAN_USES_ARPA_INET_H  1 
#endif

/* Define to 1 if you have the <byteswap.h> header file. */
#ifndef CEYLAN_USES_BYTESWAP_H 
#define CEYLAN_USES_BYTESWAP_H  1 
#endif

/* Define to 1 if you have the `bzero' function. */
#ifndef CEYLAN_USES_BZERO 
#define CEYLAN_USES_BZERO  1 
#endif

/* Define to 1 if you have the `ceilf' function. */
/* #undef CEYLAN_USES_CEILF */

/* Define to 1 if you have the `chdir' function. */
#ifndef CEYLAN_USES_CHDIR 
#define CEYLAN_USES_CHDIR  1 
#endif

/* Define to 1 if you have the <conio.h> header file. */
/* #undef CEYLAN_USES_CONIO_H */

/* Define to 1 if you have the `cosf' function. */
/* #undef CEYLAN_USES_COSF */

/* Define to 1 if you have the `ctime' function. */
#ifndef CEYLAN_USES_CTIME 
#define CEYLAN_USES_CTIME  1 
#endif

/* Define to 1 if you have the declaration of `O_APPEND', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_APPEND 
#define CEYLAN_USES_DECL_O_APPEND  1 
#endif

/* Define to 1 if you have the declaration of `O_CREAT', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_CREAT 
#define CEYLAN_USES_DECL_O_CREAT  1 
#endif

/* Define to 1 if you have the declaration of `O_EXCL', and to 0 if you don't.
   */
#ifndef CEYLAN_USES_DECL_O_EXCL 
#define CEYLAN_USES_DECL_O_EXCL  1 
#endif

/* Define to 1 if you have the declaration of `O_NONBLOCK', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_NONBLOCK 
#define CEYLAN_USES_DECL_O_NONBLOCK  1 
#endif

/* Define to 1 if you have the declaration of `O_RDONLY', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_RDONLY 
#define CEYLAN_USES_DECL_O_RDONLY  1 
#endif

/* Define to 1 if you have the declaration of `O_RDWR', and to 0 if you don't.
   */
#ifndef CEYLAN_USES_DECL_O_RDWR 
#define CEYLAN_USES_DECL_O_RDWR  1 
#endif

/* Define to 1 if you have the declaration of `O_SYNC', and to 0 if you don't.
   */
#ifndef CEYLAN_USES_DECL_O_SYNC 
#define CEYLAN_USES_DECL_O_SYNC  1 
#endif

/* Define to 1 if you have the declaration of `O_TRUNC', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_TRUNC 
#define CEYLAN_USES_DECL_O_TRUNC  1 
#endif

/* Define to 1 if you have the declaration of `O_WRONLY', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_O_WRONLY 
#define CEYLAN_USES_DECL_O_WRONLY  1 
#endif

/* Define to 1 if you have the declaration of `S_IRGRP', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_S_IRGRP 
#define CEYLAN_USES_DECL_S_IRGRP  1 
#endif

/* Define to 1 if you have the declaration of `S_IROTH', and to 0 if you
   don't. */
#ifndef CEYLAN_USES_DECL_S_IROTH 
#define CEYLAN_USES_DECL_S_IROTH  1 
#endif

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#ifndef CEYLAN_USES_DIRENT_H 
#define CEYLAN_USES_DIRENT_H  1 
#endif

/* Define to 1 if you have the <dlfcn.h> header file. */
#ifndef CEYLAN_USES_DLFCN_H 
#define CEYLAN_USES_DLFCN_H  1 
#endif

/* Define to 1 if you have the `dup' function. */
#ifndef CEYLAN_USES_DUP 
#define CEYLAN_USES_DUP  1 
#endif

/* Define to 1 if you have the `dup2' function. */
#ifndef CEYLAN_USES_DUP2 
#define CEYLAN_USES_DUP2  1 
#endif

/* Define to 1 if you have the <errno.h> header file. */
#ifndef CEYLAN_USES_ERRNO_H 
#define CEYLAN_USES_ERRNO_H  1 
#endif

/* Define to 1 if you have the `execvp' function. */
#ifndef CEYLAN_USES_EXECVP 
#define CEYLAN_USES_EXECVP  1 
#endif

/* Define to 1 if you have the `exit' function. */
#ifndef CEYLAN_USES_EXIT 
#define CEYLAN_USES_EXIT  1 
#endif

/* Define to 1 if you have the `expf' function. */
/* #undef CEYLAN_USES_EXPF */

/* Define to 1 if you have the `fabsf' function. */
/* #undef CEYLAN_USES_FABSF */

/* Define to 1 if you have the `fcntl' function. */
#ifndef CEYLAN_USES_FCNTL 
#define CEYLAN_USES_FCNTL  1 
#endif

/* Define to 1 if you have the <fcntl.h> header file. */
#ifndef CEYLAN_USES_FCNTL_H 
#define CEYLAN_USES_FCNTL_H  1 
#endif

/* Define to 1 if you have the `floorf' function. */
/* #undef CEYLAN_USES_FLOORF */

/* Define to 1 if you have the `fork' function. */
#ifndef CEYLAN_USES_FORK 
#define CEYLAN_USES_FORK  1 
#endif

/* Define to 1 if you have the `getcwd' function. */
#ifndef CEYLAN_USES_GETCWD 
#define CEYLAN_USES_GETCWD  1 
#endif

/* Define to 1 if you have the `getdomainname' function. */
#ifndef CEYLAN_USES_GETDOMAINNAME 
#define CEYLAN_USES_GETDOMAINNAME  1 
#endif

/* Define to 1 if you have the `getenv' function. */
#ifndef CEYLAN_USES_GETENV 
#define CEYLAN_USES_GETENV  1 
#endif

/* Define to 1 if you have the `geteuid' function. */
#ifndef CEYLAN_USES_GETEUID 
#define CEYLAN_USES_GETEUID  1 
#endif

/* Define to 1 if you have the `gethostbyaddr' function. */
#ifndef CEYLAN_USES_GETHOSTBYADDR 
#define CEYLAN_USES_GETHOSTBYADDR  1 
#endif

/* Define to 1 if you have the `gethostbyname' function. */
#ifndef CEYLAN_USES_GETHOSTBYNAME 
#define CEYLAN_USES_GETHOSTBYNAME  1 
#endif

/* Define to 1 if you have the `gethostname' function. */
#ifndef CEYLAN_USES_GETHOSTNAME 
#define CEYLAN_USES_GETHOSTNAME  1 
#endif

/* Define to 1 if you have the `getpid' function. */
#ifndef CEYLAN_USES_GETPID 
#define CEYLAN_USES_GETPID  1 
#endif

/* Define to 1 if you have the `getpwuid' function. */
#ifndef CEYLAN_USES_GETPWUID 
#define CEYLAN_USES_GETPWUID  1 
#endif

/* Define to 1 if you have the `gettimeofday' function. */
#ifndef CEYLAN_USES_GETTIMEOFDAY 
#define CEYLAN_USES_GETTIMEOFDAY  1 
#endif

/* Define to 1 if you have the <inttypes.h> header file. */
#ifndef CEYLAN_USES_INTTYPES_H 
#define CEYLAN_USES_INTTYPES_H  1 
#endif

/* Define to 1 if you have the `isdigit' function. */
#ifndef CEYLAN_USES_ISDIGIT 
#define CEYLAN_USES_ISDIGIT  1 
#endif

/* Define to 1 if you have the `islower' function. */
#ifndef CEYLAN_USES_ISLOWER 
#define CEYLAN_USES_ISLOWER  1 
#endif

/* Define to 1 if you have the `isupper' function. */
#ifndef CEYLAN_USES_ISUPPER 
#define CEYLAN_USES_ISUPPER  1 
#endif

/* Define to 1 if you have the `kill' function. */
#ifndef CEYLAN_USES_KILL 
#define CEYLAN_USES_KILL  1 
#endif

/* Define to 1 if you have the `labs' function. */
#ifndef CEYLAN_USES_LABS 
#define CEYLAN_USES_LABS  1 
#endif

/* Define to 1 if you have the <limits.h> header file. */
#ifndef CEYLAN_USES_LIMITS_H 
#define CEYLAN_USES_LIMITS_H  1 
#endif

/* Define to 1 if you have the `llabs' function. */
#ifndef CEYLAN_USES_LLABS 
#define CEYLAN_USES_LLABS  1 
#endif

/* Define to 1 if you have the `localtime' function. */
#ifndef CEYLAN_USES_LOCALTIME 
#define CEYLAN_USES_LOCALTIME  1 
#endif

/* Define to 1 if you have the `logf' function. */
/* #undef CEYLAN_USES_LOGF */

/* Define to 1 if you have the <ltdl.h> header file. */
#ifndef CEYLAN_USES_LTDL_H 
#define CEYLAN_USES_LTDL_H  1 
#endif

/* Define to 1 if you have the <machine/bswap.h> header file. */
/* #undef CEYLAN_USES_MACHINE_BSWAP_H */

/* Define to 1 if you have the <math.h> header file. */
#ifndef CEYLAN_USES_MATH_H 
#define CEYLAN_USES_MATH_H  1 
#endif

/* Define to 1 if you have the `memcpy' function. */
#ifndef CEYLAN_USES_MEMCPY 
#define CEYLAN_USES_MEMCPY  1 
#endif

/* Define to 1 if you have the <memory.h> header file. */
#ifndef CEYLAN_USES_MEMORY_H 
#define CEYLAN_USES_MEMORY_H  1 
#endif

/* Define to 1 if you have the `memset' function. */
#ifndef CEYLAN_USES_MEMSET 
#define CEYLAN_USES_MEMSET  1 
#endif

/* Define to 1 if you have the `mkdir' function. */
#ifndef CEYLAN_USES_MKDIR 
#define CEYLAN_USES_MKDIR  1 
#endif

/* Define to 1 if mkdir takes two arguments */
#ifndef CEYLAN_USES_MKDIR_TWO_ARGS 
#define CEYLAN_USES_MKDIR_TWO_ARGS  yes 
#endif

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef CEYLAN_USES_NDIR_H */

/* Define to 1 if you have the <netdb.h> header file. */
#ifndef CEYLAN_USES_NETDB_H 
#define CEYLAN_USES_NETDB_H  1 
#endif

/* Define to 1 if you have the <netinet/in.h> header file. */
#ifndef CEYLAN_USES_NETINET_IN_H 
#define CEYLAN_USES_NETINET_IN_H  1 
#endif

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#ifndef CEYLAN_USES_NETINET_TCP_H 
#define CEYLAN_USES_NETINET_TCP_H  1 
#endif

/* Define to 1 if you have the `pipe' function. */
#ifndef CEYLAN_USES_PIPE 
#define CEYLAN_USES_PIPE  1 
#endif

/* Define to 1 if you have the `powf' function. */
/* #undef CEYLAN_USES_POWF */

/* Define to 1 if you have the `pthread_cond_broadcast' function. */
#ifndef CEYLAN_USES_PTHREAD_COND_BROADCAST 
#define CEYLAN_USES_PTHREAD_COND_BROADCAST  1 
#endif

/* Define to 1 if you have the `pthread_cond_destroy' function. */
#ifndef CEYLAN_USES_PTHREAD_COND_DESTROY 
#define CEYLAN_USES_PTHREAD_COND_DESTROY  1 
#endif

/* Define to 1 if you have the `pthread_cond_signal' function. */
#ifndef CEYLAN_USES_PTHREAD_COND_SIGNAL 
#define CEYLAN_USES_PTHREAD_COND_SIGNAL  1 
#endif

/* Define to 1 if you have the `pthread_cond_timedwait' function. */
#ifndef CEYLAN_USES_PTHREAD_COND_TIMEDWAIT 
#define CEYLAN_USES_PTHREAD_COND_TIMEDWAIT  1 
#endif

/* Define to 1 if you have the `pthread_create' function. */
#ifndef CEYLAN_USES_PTHREAD_CREATE 
#define CEYLAN_USES_PTHREAD_CREATE  1 
#endif

/* Define to 1 if you have the <pthread.h> header file. */
#ifndef CEYLAN_USES_PTHREAD_H 
#define CEYLAN_USES_PTHREAD_H  1 
#endif

/* Define to 1 if you have the `pthread_mutex_init' function. */
#ifndef CEYLAN_USES_PTHREAD_MUTEX_INIT 
#define CEYLAN_USES_PTHREAD_MUTEX_INIT  1 
#endif

/* Define to 1 if you have the `pthread_mutex_lock' function. */
#ifndef CEYLAN_USES_PTHREAD_MUTEX_LOCK 
#define CEYLAN_USES_PTHREAD_MUTEX_LOCK  1 
#endif

/* Define to 1 if you have the `pthread_mutex_unlock' function. */
#ifndef CEYLAN_USES_PTHREAD_MUTEX_UNLOCK 
#define CEYLAN_USES_PTHREAD_MUTEX_UNLOCK  1 
#endif

/* Define to 1 if you have the `putenv' function. */
#ifndef CEYLAN_USES_PUTENV 
#define CEYLAN_USES_PUTENV  1 
#endif

/* Define to 1 if you have the <pwd.h> header file. */
#ifndef CEYLAN_USES_PWD_H 
#define CEYLAN_USES_PWD_H  1 
#endif

/* Define to 1 if you have the `raise' function. */
#ifndef CEYLAN_USES_RAISE 
#define CEYLAN_USES_RAISE  1 
#endif

/* Define to 1 if you have the `regcomp' function. */
#ifndef CEYLAN_USES_REGCOMP 
#define CEYLAN_USES_REGCOMP  1 
#endif

/* Define to 1 if you have the `regexec' function. */
#ifndef CEYLAN_USES_REGEXEC 
#define CEYLAN_USES_REGEXEC  1 
#endif

/* Define to 1 if you have the <regex.h> header file. */
#ifndef CEYLAN_USES_REGEX_H 
#define CEYLAN_USES_REGEX_H  1 
#endif

/* Define to 1 if you have the `regfree' function. */
#ifndef CEYLAN_USES_REGFREE 
#define CEYLAN_USES_REGFREE  1 
#endif

/* Define to 1 if you have the `rename' function. */
#ifndef CEYLAN_USES_RENAME 
#define CEYLAN_USES_RENAME  1 
#endif

/* Define to 1 if you have the `rmdir' function. */
#ifndef CEYLAN_USES_RMDIR 
#define CEYLAN_USES_RMDIR  1 
#endif

/* Define to 1 if you have the `round' function. */
/* #undef CEYLAN_USES_ROUND */

/* Define to 1 if you have the `roundf' function. */
/* #undef CEYLAN_USES_ROUNDF */

/* Define to 1 if you have the `roundl' function. */
/* #undef CEYLAN_USES_ROUNDL */

/* Define to 1 if you have the `setdomainname' function. */
#ifndef CEYLAN_USES_SETDOMAINNAME 
#define CEYLAN_USES_SETDOMAINNAME  1 
#endif

/* Define to 1 if you have the `setenv' function. */
#ifndef CEYLAN_USES_SETENV 
#define CEYLAN_USES_SETENV  1 
#endif

/* Define to 1 if you have the `sethostname' function. */
#ifndef CEYLAN_USES_SETHOSTNAME 
#define CEYLAN_USES_SETHOSTNAME  1 
#endif

/* Define to 1 if you have the `signal' function. */
#ifndef CEYLAN_USES_SIGNAL 
#define CEYLAN_USES_SIGNAL  1 
#endif

/* Define to 1 if you have the <signal.h> header file. */
#ifndef CEYLAN_USES_SIGNAL_H 
#define CEYLAN_USES_SIGNAL_H  1 
#endif

/* Define to 1 if you have the `sinf' function. */
/* #undef CEYLAN_USES_SINF */

/* Define to 1 if you have the `sleep' function. */
#ifndef CEYLAN_USES_SLEEP 
#define CEYLAN_USES_SLEEP  1 
#endif

/* Define to 1 if you have the `socket' function. */
#ifndef CEYLAN_USES_SOCKET 
#define CEYLAN_USES_SOCKET  1 
#endif

/* Define to 1 if you have the `sqrtf' function. */
/* #undef CEYLAN_USES_SQRTF */

/* Define to 1 if you have the `stat' function. */
#ifndef CEYLAN_USES_STAT 
#define CEYLAN_USES_STAT  1 
#endif

/* Define to 1 if `stat' has the bug that it succeeds when given the
   zero-length file name argument. */
/* #undef CEYLAN_USES_STAT_EMPTY_STRING_BUG */

/* Define to 1 if stdbool.h conforms to C99. */
#ifndef CEYLAN_USES_STDBOOL_H 
#define CEYLAN_USES_STDBOOL_H  1 
#endif

/* Define to 1 if you have the <stdint.h> header file. */
#ifndef CEYLAN_USES_STDINT_H 
#define CEYLAN_USES_STDINT_H  1 
#endif

/* Define to 1 if you have the <stdlib.h> header file. */
#ifndef CEYLAN_USES_STDLIB_H 
#define CEYLAN_USES_STDLIB_H  1 
#endif

/* Define to 1 if you have the `strdup' function. */
#ifndef CEYLAN_USES_STRDUP 
#define CEYLAN_USES_STRDUP  1 
#endif

/* Define to 1 if you have the `strerror' function. */
#ifndef CEYLAN_USES_STRERROR 
#define CEYLAN_USES_STRERROR  1 
#endif

/* Define to 1 if you have the <strings.h> header file. */
#ifndef CEYLAN_USES_STRINGS_H 
#define CEYLAN_USES_STRINGS_H  1 
#endif

/* Define to 1 if you have the <string.h> header file. */
#ifndef CEYLAN_USES_STRING_H 
#define CEYLAN_USES_STRING_H  1 
#endif

/* Define to 1 if you have the `symlink' function. */
#ifndef CEYLAN_USES_SYMLINK 
#define CEYLAN_USES_SYMLINK  1 
#endif

/* Define to 1 if you have the `sysinfo' function. */
#ifndef CEYLAN_USES_SYSINFO 
#define CEYLAN_USES_SYSINFO  1 
#endif

/* Define to 1 if you have the <sys/byteorder.h> header file. */
/* #undef CEYLAN_USES_SYS_BYTEORDER_H */

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef CEYLAN_USES_SYS_DIR_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#ifndef CEYLAN_USES_SYS_IOCTL_H 
#define CEYLAN_USES_SYS_IOCTL_H  1 
#endif

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef CEYLAN_USES_SYS_NDIR_H */

/* Define to 1 if you have the <sys/select.h> header file. */
#ifndef CEYLAN_USES_SYS_SELECT_H 
#define CEYLAN_USES_SYS_SELECT_H  1 
#endif

/* Define to 1 if you have the <sys/socket.h> header file. */
#ifndef CEYLAN_USES_SYS_SOCKET_H 
#define CEYLAN_USES_SYS_SOCKET_H  1 
#endif

/* Define to 1 if you have the <sys/stat.h> header file. */
#ifndef CEYLAN_USES_SYS_STAT_H 
#define CEYLAN_USES_SYS_STAT_H  1 
#endif

/* Define to 1 if you have the <sys/sysinfo.h> header file. */
#ifndef CEYLAN_USES_SYS_SYSINFO_H 
#define CEYLAN_USES_SYS_SYSINFO_H  1 
#endif

/* Define to 1 if you have the <sys/systeminfo.h> header file. */
/* #undef CEYLAN_USES_SYS_SYSTEMINFO_H */

/* Define to 1 if you have the <sys/times.h> header file. */
#ifndef CEYLAN_USES_SYS_TIMES_H 
#define CEYLAN_USES_SYS_TIMES_H  1 
#endif

/* Define to 1 if you have the <sys/time.h> header file. */
#ifndef CEYLAN_USES_SYS_TIME_H 
#define CEYLAN_USES_SYS_TIME_H  1 
#endif

/* Define to 1 if you have the <sys/types.h> header file. */
#ifndef CEYLAN_USES_SYS_TYPES_H 
#define CEYLAN_USES_SYS_TYPES_H  1 
#endif

/* Define to 1 if you have the <sys/utsname.h> header file. */
#ifndef CEYLAN_USES_SYS_UTSNAME_H 
#define CEYLAN_USES_SYS_UTSNAME_H  1 
#endif

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#ifndef CEYLAN_USES_SYS_WAIT_H 
#define CEYLAN_USES_SYS_WAIT_H  1 
#endif

/* Define to 1 if you have the <termios.h> header file. */
#ifndef CEYLAN_USES_TERMIOS_H 
#define CEYLAN_USES_TERMIOS_H  1 
#endif

/* Define to 1 if you have the `time' function. */
#ifndef CEYLAN_USES_TIME 
#define CEYLAN_USES_TIME  1 
#endif

/* Define to 1 if you have the `times' function. */
#ifndef CEYLAN_USES_TIMES 
#define CEYLAN_USES_TIMES  1 
#endif

/* Define to 1 if you have the <time.h> header file. */
#ifndef CEYLAN_USES_TIME_H 
#define CEYLAN_USES_TIME_H  1 
#endif

/* Define to 1 if you have the `uname' function. */
#ifndef CEYLAN_USES_UNAME 
#define CEYLAN_USES_UNAME  1 
#endif

/* Define to 1 if you have the <unistd.h> header file. */
#ifndef CEYLAN_USES_UNISTD_H 
#define CEYLAN_USES_UNISTD_H  1 
#endif

/* Define to 1 if you have the `unlink' function. */
#ifndef CEYLAN_USES_UNLINK 
#define CEYLAN_USES_UNLINK  1 
#endif

/* Define to 1 if you have the `unsetenv' function. */
#ifndef CEYLAN_USES_UNSETENV 
#define CEYLAN_USES_UNSETENV  1 
#endif

/* Define to 1 if you have the `utime' function. */
#ifndef CEYLAN_USES_UTIME 
#define CEYLAN_USES_UTIME  1 
#endif

/* Define to 1 if you have the <utime.h> header file. */
#ifndef CEYLAN_USES_UTIME_H 
#define CEYLAN_USES_UTIME_H  1 
#endif

/* Define to 1 if `utime(file, NULL)' sets file's timestamp to the present. */
#ifndef CEYLAN_USES_UTIME_NULL 
#define CEYLAN_USES_UTIME_NULL  1 
#endif

/* Define to 1 if you have the `vfork' function. */
#ifndef CEYLAN_USES_VFORK 
#define CEYLAN_USES_VFORK  1 
#endif

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef CEYLAN_USES_VFORK_H */

/* Define to 1 if you have the `waitpid' function. */
#ifndef CEYLAN_USES_WAITPID 
#define CEYLAN_USES_WAITPID  1 
#endif

/* Define to 1 if `fork' works. */
#ifndef CEYLAN_USES_WORKING_FORK 
#define CEYLAN_USES_WORKING_FORK  1 
#endif

/* Define to 1 if `vfork' works. */
#ifndef CEYLAN_USES_WORKING_VFORK 
#define CEYLAN_USES_WORKING_VFORK  1 
#endif

/* Define to 1 if you have the <xti.h> header file. */
/* #undef CEYLAN_USES_XTI_H */

/* Define to 1 if the system has the type `_Bool'. */
#ifndef CEYLAN_USES__BOOL 
#define CEYLAN_USES__BOOL  1 
#endif

/* Current Libtool version for the Ceylan library */
#ifndef CEYLAN_LIBTOOL_VERSION 
#define CEYLAN_LIBTOOL_VERSION  "0.7.0" 
#endif

/* Define to 1 if `lstat' dereferences a symlink specified with a trailing
   slash. */
#ifndef CEYLAN_LSTAT_FOLLOWS_SLASHED_SYMLINK 
#define CEYLAN_LSTAT_FOLLOWS_SLASHED_SYMLINK  1 
#endif

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#ifndef CEYLAN_LT_OBJDIR 
#define CEYLAN_LT_OBJDIR  ".libs/" 
#endif

/* Name of package */
#ifndef CEYLAN_PACKAGE 
#define CEYLAN_PACKAGE  "ceylan" 
#endif

/* Define to the address where bug reports for this package should be sent. */
#ifndef CEYLAN_PACKAGE_BUGREPORT 
#define CEYLAN_PACKAGE_BUGREPORT  "ceylan-bugs@lists.sourceforge.net" 
#endif

/* Define to the full name of this package. */
#ifndef CEYLAN_PACKAGE_NAME 
#define CEYLAN_PACKAGE_NAME  "Ceylan" 
#endif

/* Define to the full name and version of this package. */
#ifndef CEYLAN_PACKAGE_STRING 
#define CEYLAN_PACKAGE_STRING  "Ceylan 0.7" 
#endif

/* Define to the one symbol short name of this package. */
#ifndef CEYLAN_PACKAGE_TARNAME 
#define CEYLAN_PACKAGE_TARNAME  "ceylan" 
#endif

/* Define to the version of this package. */
#ifndef CEYLAN_PACKAGE_VERSION 
#define CEYLAN_PACKAGE_VERSION  "0.7" 
#endif

/* Define as the return type of signal handlers (`int' or `void'). */
#ifndef CEYLAN_RETSIGTYPE 
#define CEYLAN_RETSIGTYPE  void 
#endif

/* Define to 1 if the target platform is little endian */
#ifndef CEYLAN_RUNS_ON_LITTLE_ENDIAN 
#define CEYLAN_RUNS_ON_LITTLE_ENDIAN  1 
#endif

/* Define to the type of arg 1 for `select'. */
#ifndef CEYLAN_SELECT_TYPE_ARG1 
#define CEYLAN_SELECT_TYPE_ARG1  int 
#endif

/* Define to the type of args 2, 3 and 4 for `select'. */
#ifndef CEYLAN_SELECT_TYPE_ARG234 
#define CEYLAN_SELECT_TYPE_ARG234  (fd_set *) 
#endif

/* Define to the type of arg 5 for `select'. */
#ifndef CEYLAN_SELECT_TYPE_ARG5 
#define CEYLAN_SELECT_TYPE_ARG5  (struct timeval *) 
#endif

/* Define to 1 if you have the ANSI C header files. */
#ifndef CEYLAN_STDC_HEADERS 
#define CEYLAN_STDC_HEADERS  1 
#endif

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#ifndef CEYLAN_TIME_WITH_SYS_TIME 
#define CEYLAN_TIME_WITH_SYS_TIME  1 
#endif

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef CEYLAN_TM_IN_SYS_TIME */

/* Define to 1 if Ceylan is to provide advanced file attribute support */
#ifndef CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES 
#define CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES  1 
#endif

/* Define to 1 if Ceylan is to support avanced process management */
#ifndef CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT 
#define CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT  1 
#endif

/* Define to 1 if fcntl is to be preferred to ioctl */
#ifndef CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS 
#define CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS  1 
#endif

/* Define to 1 if Ceylan is to provide file descriptor support */
#ifndef CEYLAN_USES_FILE_DESCRIPTORS 
#define CEYLAN_USES_FILE_DESCRIPTORS  1 
#endif

/* Define to 1 if Ceylan is to provide file lock support */
#ifndef CEYLAN_USES_FILE_LOCKS 
#define CEYLAN_USES_FILE_LOCKS  1 
#endif

/* Define to 1 if Ceylan is to provide network support */
#ifndef CEYLAN_USES_NETWORK 
#define CEYLAN_USES_NETWORK  1 
#endif

/* Define to 1 if Ceylan is to support plugin */
#ifndef CEYLAN_USES_PLUGINS 
#define CEYLAN_USES_PLUGINS  1 
#endif

/* Define to 1 if Ceylan is to use regular expressions internally */
#ifndef CEYLAN_USES_REGEX 
#define CEYLAN_USES_REGEX  1 
#endif

/* Define to 1 if Ceylan is to support signals */
#ifndef CEYLAN_USES_SIGNALS 
#define CEYLAN_USES_SIGNALS  1 
#endif

/* Define to 1 if Ceylan is to provide symbolic link support */
#ifndef CEYLAN_USES_SYMBOLIC_LINKS 
#define CEYLAN_USES_SYMBOLIC_LINKS  1 
#endif

/* Define to 1 if Ceylan is to provide multithreading support */
#ifndef CEYLAN_USES_THREADS 
#define CEYLAN_USES_THREADS  1 
#endif

/* Version number of package */
#ifndef CEYLAN_VERSION 
#define CEYLAN_VERSION  "0.7" 
#endif

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef _ceylan_const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef _ceylan_inline */
#endif

/* Define to `int' if <sys/types.h> does not define. */
/* #undef _ceylan_pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef _ceylan_size_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef _ceylan_vfork */

/* Define to empty if the keyword `volatile' does not work. Warning: valid
   code using `volatile' can become incorrect without. Disable with care. */
/* #undef _ceylan_volatile */
 
/* once: _SRC_CODE_CEYLANCONFIG_H */
#endif
/* Note: NEVER commit (check-in) this generated file ! */
