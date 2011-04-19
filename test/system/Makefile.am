SUFFIXES = .cc .h


testssystemdir=@CEYLAN_TEST_DIR@/system


testssystem_PROGRAMS = \
	testCeylanConsole.exe                      \
	testCeylanEnvironmentVariable.exe          \
	testCeylanEnvironmentVariable.exe          \
	testCeylanFileAndDirectories.exe           \
	testCeylanFileLocator.exe                  \
	testCeylanMemoryStream.exe                 \
	testCeylanSignals.exe                      \
	testCeylanSystemInformation.exe            \
	testCeylanSystem.exe                       \
	testCeylanThread.exe                       \
	testCeylanTime.exe


testCeylanConsole_exe_SOURCES             = testCeylanConsole.cc
testCeylanEnvironmentVariable_exe_SOURCES = testCeylanEnvironmentVariable.cc
testCeylanFileAndDirectories_exe_SOURCES  = testCeylanFileAndDirectories.cc
testCeylanFileLocator_exe_SOURCES         = testCeylanFileLocator.cc
testCeylanMemoryStream_exe_SOURCES        = testCeylanMemoryStream.cc
testCeylanSignals_exe_SOURCES             = testCeylanSignals.cc
testCeylanSystemInformation_exe_SOURCES   = testCeylanSystemInformation.cc
testCeylanSystem_exe_SOURCES              = testCeylanSystem.cc
testCeylanThread_exe_SOURCES              = testCeylanThread.cc
testCeylanTime_exe_SOURCES                = testCeylanTime.cc

clean: clean-local

clean-local:
	-@/bin/rm -f schedulingGranularity.png testCeylanTime-granularity.dat
