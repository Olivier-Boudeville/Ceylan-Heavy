## Makefile.am for the 'Middleware' module of the Ceylan library.


# Convenience library for the 'Middleware' module :
noinst_LTLIBRARIES = libCeylanMiddleware.la

libCeylanMiddleware_la_SOURCES = $(MIDDLEWARE_INTERFACES) \
	$(MIDDLEWARE_IMPLEMENTATIONS)

libCeylanMiddleware_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_SYSTEM@ -I@CEYLAN_MODULES@

libCeylanMiddlewareIncludedir      = $(includedir)/Ceylan
libCeylanMiddlewareInclude_HEADERS = $(MIDDLEWARE_INTERFACES)


MIDDLEWARE_INTERFACES = \
	CeylanLightWeightMarshaller.h        \
	CeylanMarshaller.h                   \
	CeylanMiddleware.h                   \
	CeylanMiddlewareIncludes.h           \
	CeylanProtocolEndpoint.h             \
	CeylanProtocolServer.h
	

MIDDLEWARE_IMPLEMENTATIONS = \
	CeylanLightWeightMarshaller.cc       \
	CeylanMarshaller.cc                  \
	CeylanMiddleware.cc                  \
	CeylanProtocolEndpoint.cc            \
	CeylanProtocolServer.cc

