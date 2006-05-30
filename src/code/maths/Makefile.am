## Makefile.am for the 'Maths' module of the Ceylan library.

# Convenience library for the 'Maths' module :
noinst_LTLIBRARIES = libCeylanMaths.la

libCeylanMaths_la_SOURCES = $(MATHS_INTERFACES) $(MATHS_IMPLEMENTATIONS)

libCeylanMaths_la_CPPFLAGS = -I@CEYLAN_GENERIC@ -I@CEYLAN_LOGS@ -I@CEYLAN_INTERFACES@ -I@CEYLAN_SYSTEM@ -I@CEYLAN_MODULES@

libCeylanMathsIncludedir      = $(includedir)/Ceylan
libCeylanMathsInclude_HEADERS = $(MATHS_INTERFACES)


MATHS_INTERFACES = \
	CeylanBipoint.h                           \
	CeylanEndomorphism.h                      \
	CeylanHomogeneousMatrix3.h                \
	CeylanLinear.h                            \
	CeylanMathsBasic.h                        \
	CeylanMathsIncludes.h                     \
	CeylanMatrix.h                            \
	CeylanMatrix2.h                           \
	CeylanMatrix3.h                           \
	CeylanNormalProbability.h                 \
	CeylanPoint.h                             \
	CeylanProbability.h                       \
	CeylanRandomGenerator.h                   \
	CeylanRandomGeneratorFromPDF.h            \
	CeylanTripoint.h                          \
	CeylanVector.h                            \
	CeylanVector2.h                           \
	CeylanVector3.h                           \
	CeylanWhiteNoiseRandomGenerator.h


MATHS_IMPLEMENTATIONS = \
	CeylanBipoint.cc                          \
	CeylanEndomorphism.cc                     \
	CeylanHomogeneousMatrix3.cc               \
	CeylanLinear.cc                           \
	CeylanMathsBasic.cc                       \
	CeylanMatrix.cc                           \
	CeylanMatrix2.cc                          \
	CeylanMatrix3.cc                          \
	CeylanNormalProbability.cc                \
	CeylanPoint.cc                            \
	CeylanProbability.cc                      \
	CeylanRandomGenerator.cc                  \
	CeylanRandomGeneratorFromPDF.cc           \
	CeylanTripoint.cc                         \
	CeylanVector.cc                           \
	CeylanVector2.cc                          \
	CeylanVector3.cc                          \
	CeylanWhiteNoiseRandomGenerator.cc

