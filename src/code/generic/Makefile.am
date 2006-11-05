## Makefile.am for the 'Generic' module of the Ceylan library.


# Convenience library for the 'Generic' module :
noinst_LTLIBRARIES = libCeylanGeneric.la

libCeylanGeneric_la_SOURCES = $(GENERIC_INTERFACES) $(GENERIC_IMPLEMENTATIONS)

libCeylanGeneric_la_CPPFLAGS = -I@CEYLAN_LOGS@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_SYSTEM@ -I@CEYLAN_MODULES@

libCeylanGenericIncludedir      = $(includedir)/Ceylan
libCeylanGenericInclude_HEADERS = $(GENERIC_INTERFACES)


GENERIC_INTERFACES = \
	CeylanBasicResourceManager.h         \
	CeylanCountedPointer.h               \
	CeylanDocMainPage.h                  \
	CeylanException.h                    \
	CeylanGenericIncludes.h              \
	CeylanHeaderVersion.h                \
	CeylanLibtoolVersion.h               \
	CeylanOperators.h                    \
	CeylanRegularExpression.h            \
	CeylanResource.h                     \
	CeylanResourceManager.h              \
	CeylanSingleton.h                    \
	CeylanSmartResource.h                \
	CeylanSmartResourceManager.h         \
	CeylanStringUtils.h                  \
	CeylanTestException.h                \
	CeylanTimestamp.h                    \
	CeylanTree.h                         \
	CeylanTypes.h                        \
	CeylanUnicode.h                      \
	CeylanUtils.h                        \
	CeylanVersion.h


GENERIC_IMPLEMENTATIONS = \
	CeylanException.cc                   \
	CeylanHeaderVersion.cc               \
	CeylanLibtoolVersion.cc              \
	CeylanOperators.cc                   \
	CeylanRegularExpression.cc           \
	CeylanResource.cc                    \
	CeylanResourceManager.cc             \
	CeylanSingleton.cc                   \
	CeylanSmartResource.cc               \
	CeylanStringUtils.cc                 \
	CeylanTestException.cc               \
	CeylanTimestamp.cc                   \
	CeylanTree.cc                        \
	CeylanTypes.cc                       \
	CeylanUnicode.cc                     \
	CeylanUtils.cc                       \
	CeylanVersion.cc

	 
# DISABLED = CeylanSharedPointer.h

