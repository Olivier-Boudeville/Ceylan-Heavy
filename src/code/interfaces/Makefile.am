## Makefile.am for the 'Interfaces' module of the Ceylan library.


# Convenience library for the 'Interfaces' module :
noinst_LTLIBRARIES = libCeylanInterfaces.la

libCeylanInterfaces_la_SOURCES = $(INTERFACES_INTERFACES) $(INTERFACES_IMPLEMENTATIONS)

libCeylanInterfaces_la_CPPFLAGS = -I@CEYLAN_GENERIC@ -I@CEYLAN_LOGS@ -I@CEYLAN_SYSTEM@ -I@CEYLAN_MATHS@ -I@CEYLAN_MODULES@ -I@CEYLAN_NETWORK@

libCeylanInterfacesIncludedir      = $(includedir)/Ceylan
libCeylanInterfacesInclude_HEADERS = $(INTERFACES_INTERFACES)


INTERFACES_INTERFACES = \
	CeylanCallableEventSource.h                   \
	CeylanCallerEventListener.h                   \
	CeylanClonable.h                              \
	CeylanController.h                            \
	CeylanCountable.h                             \
	CeylanDisplayable.h                           \
	CeylanEvent.h                                 \
	CeylanEventListener.h                         \
	CeylanEventSource.h                           \
	CeylanHashable.h                              \
	CeylanIdentifier.h                            \
	CeylanIdentifierOwner.h                       \
	CeylanInputDevice.h                           \
	CeylanInterfacesIncludes.h                    \
	CeylanLocatable.h                             \
	CeylanLocatable2D.h                           \
	CeylanLockable.h                              \
	CeylanLoggable.h                              \
	CeylanMeasurable.h                            \
	CeylanModel.h                                 \
	CeylanObject.h                                \
	CeylanObjectChannel.h                         \
	CeylanObjectIdentifier.h                      \
	CeylanSerializable.h                          \
	CeylanTextDisplayable.h                       \
	CeylanTextIdentifier.h                        \
	CeylanView.h                                  \
	CeylanVisitable.h                             \
	CeylanVisitor.h


INTERFACES_IMPLEMENTATIONS = \
	CeylanCallableEventSource.cc                  \
	CeylanCallerEventListener.cc                  \
	CeylanClonable.cc                             \
	CeylanController.cc                           \
	CeylanCountable.cc                            \
	CeylanEvent.cc                                \
	CeylanEventListener.cc                        \
	CeylanEventSource.cc                          \
	CeylanHashable.cc                             \
	CeylanIdentifier.cc                           \
	CeylanIdentifierOwner.cc                      \
	CeylanInputDevice.cc                          \
	CeylanLocatable.cc                            \
	CeylanLocatable2D.cc                          \
	CeylanLockable.cc                             \
	CeylanLoggable.cc                             \
	CeylanMeasurable.cc                           \
	CeylanModel.cc                                \
	CeylanObject.cc                               \
	CeylanObjectChannel.cc                        \
	CeylanObjectIdentifier.cc                     \
	CeylanSerializable.cc                         \
	CeylanTextDisplayable.cc                      \
	CeylanTextIdentifier.cc                       \
	CeylanView.cc                                 \
	CeylanVisitable.cc                            \
	CeylanVisitor.cc

