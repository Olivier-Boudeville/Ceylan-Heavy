SUFFIXES = .cc .h


testsmodulesdir=@CEYLAN_TEST_DIR@/modules


testsmodules_PROGRAMS = \
	testCeylanFeatures.exe   \
	testCeylanModule.exe     \
	testCeylanPlugin.exe     \
	testCeylanXMLParser.exe

testCeylanFeatures_exe_SOURCES    = testCeylanFeatures.cc
testCeylanModule_exe_SOURCES      = testCeylanModule.cc

testCeylanPlugin_exe_SOURCES      = testCeylanPlugin.cc
testCeylanPlugin_exe_CPPFLAGS     = $(LTDLINCL)


# If symbols from your executable are needed to satisfy unresolved 
# references in a library you want to dlopen, you will have to use the flag
# -export-dynamic :
testCeylanPlugin_exe_LDFLAGS = -export-dynamic -no-undefined -dlpreopen force -dlopen self -dlopen ceylan-test-plugin.la

# The shared library that may be plugged-in must be specified when linking 
# the executable, so that libtool can dlpreopen the library on platforms 
# which do not support dlopening :
testCeylanPlugin_exe_LDADD   = $(LIBLTDL) @CEYLAN_LIBSTDCPLUSPLUS@

testCeylanPlugin_exe_DEPENDENCIES = $(LIBLTDL) ceylan-test-plugin.la


ceylan_test_plugin_la_SOURCES = ceylan-test-plugin.cc
ceylan_test_plugin_la_LDFLAGS = -no-undefined -module -avoid-version

ceylan_test_unknown_plugin_la_SOURCES = ceylan-test-unknown-plugin.cc
ceylan_test_unknown_plugin_la_LDFLAGS = -no-undefined -module -avoid-version

# The unknown plugin must be in the same directory as the test loading it
# to be easily found :
testsmodules_LTLIBRARIES = ceylan-test-plugin.la ceylan-test-unknown-plugin.la


testCeylanXMLParser_exe_SOURCES   = testCeylanXMLParser.cc


