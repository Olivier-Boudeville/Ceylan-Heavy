## Makefile.am for the 'Middleware' module of the Ceylan library.


# Convenience library for the 'Middleware' module :
noinst_LTLIBRARIES = libCeylanMiddleware.la

libCeylanMiddleware_la_SOURCES = $(MIDDLEWARE_INTERFACES) \
	$(MIDDLEWARE_IMPLEMENTATIONS)

libCeylanMiddleware_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@

libCeylanMiddlewareIncludedir      = $(includedir)/Ceylan
libCeylanMiddlewareInclude_HEADERS = $(MIDDLEWARE_INTERFACES)


MIDDLEWARE_INTERFACES = \
	CeylanMiddlewareIncludes.h           \
	CeylanProtocolServer.h

MIDDLEWARE_IMPLEMENTATIONS = \
	CeylanProtocolServer.cc

