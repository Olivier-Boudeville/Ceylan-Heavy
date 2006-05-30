## Makefile.am for the 'Modules' module of the Ceylan library.


# Convenience library for the 'Modules' module :
noinst_LTLIBRARIES = libCeylanModules.la

libCeylanModules_la_SOURCES = $(MODULES_INTERFACES) $(MODULES_IMPLEMENTATIONS)

libCeylanModules_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_GENERIC@ -I@CEYLAN_INTERFACES@

libCeylanModulesIncludedir      = $(includedir)/Ceylan
libCeylanModulesInclude_HEADERS = $(MODULES_INTERFACES)


MODULES_INTERFACES = \
	CeylanFeatures.h                          \
	CeylanModule.h                            \
	CeylanModulesIncludes.h


MODULES_IMPLEMENTATIONS = \
	CeylanFeatures.cc                         \
	CeylanModule.cc

	
