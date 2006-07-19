## Makefile.am for the 'System' module of the Ceylan library.


# Convenience library for the 'System' module :
noinst_LTLIBRARIES = libCeylanSystem.la

libCeylanSystem_la_SOURCES = $(SYSTEM_INTERFACES) $(SYSTEM_IMPLEMENTATIONS)

libCeylanSystem_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_MATHS@ -I@CEYLAN_MODULES@

libCeylanSystemIncludedir      = $(includedir)/Ceylan
libCeylanSystemInclude_HEADERS = $(SYSTEM_INTERFACES)


SYSTEM_INTERFACES = \
	CeylanAnonymousInputStream.h         \
	CeylanDirectory.h                    \
	CeylanEnvironmentVariables.h         \
	CeylanFile.h                         \
	CeylanFileLocator.h                  \
	CeylanInputStream.h                  \
	CeylanMutex.h                        \
	CeylanOutputStream.h                 \
	CeylanPipe.h                         \
	CeylanProcess.h                      \
	CeylanRunnable.h                     \
	CeylanSignal.h                       \
	CeylanStream.h                       \
	CeylanSynchronized.h                 \
	CeylanSystem.h                       \
	CeylanSystemIncludes.h               \
	CeylanSystemInformation.h            \
	CeylanThread.h                       \
	CeylanUniformResourceIdentifier.h


SYSTEM_IMPLEMENTATIONS = \
	CeylanAnonymousInputStream.cc        \
	CeylanDirectory.cc                   \
	CeylanEnvironmentVariables.cc        \
	CeylanFile.cc                        \
	CeylanFileLocator.cc                 \
	CeylanInputStream.cc                 \
	CeylanMutex.cc                       \
	CeylanOutputStream.cc                \
	CeylanPipe.cc                        \
	CeylanProcess.cc                     \
	CeylanRunnable.cc                    \
	CeylanSignal.cc                      \
	CeylanStream.cc                      \
	CeylanSystem.cc                      \
	CeylanSystemInformation.cc           \
	CeylanThread.cc                      \
	CeylanUniformResourceIdentifier.cc

