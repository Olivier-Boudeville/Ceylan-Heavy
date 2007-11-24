## Makefile.am for the 'System' module of the Ceylan library.


# Convenience library for the 'System' module:
noinst_LTLIBRARIES = libCeylanSystem.la

libCeylanSystem_la_SOURCES = $(SYSTEM_INTERFACES) $(SYSTEM_IMPLEMENTATIONS)

libCeylanSystem_la_CPPFLAGS = -I@CEYLAN_GENERIC@ -I@CEYLAN_LOGS@  -I@CEYLAN_INTERFACES@ -I@CEYLAN_MATHS@ -I@CEYLAN_MODULES@ -I@CEYLAN_NETWORK@

libCeylanSystemIncludedir      = $(includedir)/Ceylan
libCeylanSystemInclude_HEADERS = $(SYSTEM_INTERFACES)


noinst_HEADERS = \
	CeylanEndianness.h


SYSTEM_INTERFACES = \
	CeylanAnonymousInputOutputStream.h   \
	CeylanAnonymousInputStream.h         \
	CeylanARM7Codes.h                    \
	CeylanConsole.h                      \
	CeylanDirectory.h                    \
	CeylanEnvironmentVariables.h         \
	CeylanFIFO.h                         \
	CeylanFile.h                         \
	CeylanFileLocator.h                  \
	CeylanFileSystemCommon.h             \
	CeylanFileSystemManager.h            \
	CeylanInputOutputStream.h            \
	CeylanInputStream.h                  \
	CeylanIPCCommands.h                  \
	CeylanLibfatDirectory.h              \
	CeylanLibfatFile.h                   \
	CeylanLibfatFileSystemManager.h      \
	CeylanMemoryStream.h                 \
	CeylanMutex.h                        \
	CeylanOutputStream.h                 \
	CeylanPipe.h                         \
	CeylanProcess.h                      \
	CeylanRunnable.h                     \
	CeylanSignal.h                       \
	CeylanStandardDirectory.h            \
	CeylanStandardFile.h                 \
	CeylanStandardFileSystemManager.h    \
	CeylanStream.h                       \
	CeylanSynchronized.h                 \
	CeylanSystem.h                       \
	CeylanSystemIncludes.h               \
	CeylanSystemInformation.h            \
	CeylanThread.h                       \
	CeylanUniformResourceIdentifier.h


SYSTEM_IMPLEMENTATIONS = \
	CeylanAnonymousInputOutputStream.cc  \
	CeylanAnonymousInputStream.cc        \
	CeylanConsole.cc                     \
	CeylanDirectory.cc                   \
	CeylanEnvironmentVariables.cc        \
	CeylanFIFO.cc                        \
	CeylanFile.cc                        \
	CeylanFileLocator.cc                 \
	CeylanFileSystemManager.cc           \
	CeylanInputOutputStream.cc           \
	CeylanInputStream.cc                 \
	CeylanLibfatDirectory.cc             \
	CeylanLibfatFile.cc                  \
	CeylanLibfatFileSystemManager.cc     \
	CeylanMemoryStream.cc                \
	CeylanMutex.cc                       \
	CeylanOutputStream.cc                \
	CeylanPipe.cc                        \
	CeylanProcess.cc                     \
	CeylanRunnable.cc                    \
	CeylanSignal.cc                      \
	CeylanStandardDirectory.cc           \
	CeylanStandardFile.cc                \
	CeylanStandardFileSystemManager.cc   \
	CeylanStream.cc                      \
	CeylanSystem.cc                      \
	CeylanSystemInformation.cc           \
	CeylanThread.cc                      \
	CeylanUniformResourceIdentifier.cc

