## Makefile.am for the main Ceylan library.


SUFFIXES = .cc .h


# These are the subdirectories (i.e. submodules) that are always built :
BUILT_SUBDIRS = \
	generic     \
	interfaces  \
	logs        \
	maths       \
	middleware  \
	modules     \
	network     \
	system      

	
	
# These are the subdirectories which may be built :
EXTRA_SUBDIRS =	@CEYLAN_EXTRA_DIRS@

# Subdirectories taken into account but not especially built :
OTHER_SUBDIRS = scripts

# These are the subdirectories which will be taken into account now :
SUBDIRS = $(BUILT_SUBDIRS) $(OTHER_SUBDIRS) $(EXTRA_SUBDIRS)

# These are the subdirectories which will be distributed with 'make dist' :
DIST_SUBDIRS = $(SUBDIRS)


# The Ceylan library target :
lib_LTLIBRARIES = libCeylan.la

# All the library sources :
libCeylan_la_SOURCES = $(GENERAL_SRCS)

# Linker flag for the library :
libCeylan_la_LDFLAGS = \
	-no-undefined                                                            \
	-release @CEYLAN_LT_RELEASE@                                             \
	-version-info @CEYLAN_LT_CURRENT@:@CEYLAN_LT_REVISION@:@CEYLAN_LT_AGE@ 

				
# Submodule convenience libraries :	
SUBLIBS = \
	generic/libCeylanGeneric.la       \
	interfaces/libCeylanInterfaces.la \
	logs/libCeylanLogs.la             \
	maths/libCeylanMaths.la           \
	middleware/libCeylanMiddleware.la \
	modules/libCeylanModules.la       \
	network/libCeylanNetwork.la       \
	system/libCeylanSystem.la
	

EXTRA_SUBLIBS = @CEYLAN_EXTRA_LIBLTDL_SUBLIB@	
	
# The overall library is made from submodule convenience libraries and al :	
libCeylan_la_LIBADD	= \
	$(SUBLIBS)           \
	@CEYLAN_EXTRA_LIBS@  \
	@SYSTEM_LIBS@        
	
	
libCeylan_la_DEPENDENCIES = \
	$(SUBLIBS)           \
	@CEYLAN_EXTRA_LIBS@   

	
libCeylanIncludedir      = $(includedir)/Ceylan
libCeylanInclude_HEADERS = Ceylan.h

DISTCLEANFILES = CeylanConfig.h
	
	
# Run ldconfig after installing the library:
install-hook:
	-ldconfig

clean-local:
	-@rm -f stamp-h1 CeylanTemporaryConfig.h*
