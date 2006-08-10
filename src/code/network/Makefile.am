## Makefile.am for the 'Network' module of the Ceylan library.


# Convenience library for the 'Network' module :
noinst_LTLIBRARIES = libCeylanNetwork.la

libCeylanNetwork_la_SOURCES = $(NETWORK_INTERFACES) $(NETWORK_IMPLEMENTATIONS)

libCeylanNetwork_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_MODULES@ -I@CEYLAN_SYSTEM@

libCeylanNetworkIncludedir      = $(includedir)/Ceylan
libCeylanNetworkInclude_HEADERS = $(NETWORK_INTERFACES)

noinst_HEADERS = CeylanSystemSpecificSocketAddress.h


NETWORK_INTERFACES = \
	CeylanAnonymousStreamSocket.h	           \
	CeylanClientStreamSocket.h                 \
	CeylanIPAddress.h                          \
	CeylanIPAddressvFour.h                     \
	CeylanMultiplexedServerStreamSocket.h      \
	CeylanNetwork.h                            \
	CeylanNetworkIncludes.h                    \
	CeylanSequentialServerStreamSocket.h       \
	CeylanServerStreamSocket.h                 \
	CeylanSocket.h                             \
	CeylanStreamSocket.h                       



NETWORK_IMPLEMENTATIONS = \
	CeylanAnonymousStreamSocket.cc	           \
	CeylanClientStreamSocket.cc                \
	CeylanIPAddress.cc                         \
	CeylanIPAddressvFour.cc                    \
	CeylanMultiplexedServerStreamSocket.cc     \
	CeylanNetwork.cc                           \
	CeylanSequentialServerStreamSocket.cc      \
	CeylanServerStreamSocket.cc                \
	CeylanSocket.cc                            \
	CeylanStreamSocket.cc                       

