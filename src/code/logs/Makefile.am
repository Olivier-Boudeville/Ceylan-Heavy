## Makefile.am for the 'Logs' module of the Ceylan library.


# Convenience library for the 'Logs' module :
noinst_LTLIBRARIES = libCeylanLogs.la

libCeylanLogs_la_SOURCES = $(LOGS_INTERFACES) $(LOGS_IMPLEMENTATIONS)

libCeylanLogs_la_CPPFLAGS = -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_SYSTEM@ -I@CEYLAN_MODULES@

libCeylanLogsIncludedir      = $(includedir)/Ceylan
libCeylanLogsInclude_HEADERS = $(LOGS_INTERFACES)

noinst_HEADERS = CeylanLogLight.h CeylanLogAggregatorHTMLFragments.h


LOGS_INTERFACES = \
	CeylanLogAggregatorConsole.h         \
	CeylanLogAggregator.h                \
	CeylanLogAggregatorHTML.h            \
	CeylanLogAggregatorRaw.h             \
	CeylanLogChannel.h                   \
	CeylanLog.h                          \
	CeylanLogHolder.h                    \
	CeylanLogIncludes.h                  \
	CeylanLogListener.h                  \
	CeylanLogMessage.h                   \
	CeylanLogPlugClassical.h             \
	CeylanLogPlugConsole.h               \
	CeylanLogPlug.h                      \
	CeylanLogPlugHTML.h                  \
	CeylanLogPlugNull.h                  \
	CeylanLogSource.h                    \
	CeylanLogTransport.h                 \
	CeylanMuteLogSource.h                \
	CeylanLogTransportListenerRaw.h
	     
	     
	     
LOGS_IMPLEMENTATIONS = \
	CeylanLogAggregator.cc               \
	CeylanLogAggregatorConsole.cc        \
	CeylanLogAggregatorHTML.cc           \
	CeylanLogAggregatorRaw.cc            \
	CeylanLog.cc                         \
	CeylanLogChannel.cc                  \
	CeylanLogHolder.cc                   \
	CeylanLogListener.cc                 \
	CeylanLogMessage.cc                  \
	CeylanLogPlug.cc                     \
	CeylanLogPlugClassical.cc            \
	CeylanLogPlugConsole.cc              \
	CeylanLogPlugHTML.cc                 \
	CeylanLogPlugNull.cc                 \
	CeylanLogSource.cc                   \
	CeylanLogTransport.cc                \
	CeylanMuteLogSource.cc               \
	CeylanLogTransportListenerRaw.cc

