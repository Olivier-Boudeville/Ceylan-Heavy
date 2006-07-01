SUFFIXES = .cc .h


testsmodulesdir=@CEYLAN_TEST_DIR@/modules


testsmodules_PROGRAMS = \
	testCeylanFeatures \
	testCeylanModule   \
	testCeylanPlugin


testCeylanFeatures_SOURCES = testCeylanFeatures.cc
testCeylanModule_SOURCES   = testCeylanModule.cc

testCeylanPlugin_SOURCES   = testCeylanPlugin.cc
testCeylanPlugin_LDFLAGS   = -export-dynamic



ceylan-test-plugin.so: ceylan-test-plugin.o


