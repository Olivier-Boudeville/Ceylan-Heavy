SUFFIXES = .cc .h


install-data-local:
	@echo; echo " Installing playTests-local.sh"
	@mkdir -p $(DESTDIR)@CEYLAN_TEST_DIR@/network
	@cp -f $(srcdir)/playTests-local.sh $(DESTDIR)@CEYLAN_TEST_DIR@/network


testsnetworkdir=@CEYLAN_TEST_DIR@/network


testsnetwork_PROGRAMS = \
	testCeylanClientStream.exe                 \
	testCeylanIPAddressvFour.exe               \
	testCeylanMultiplexedServerStream.exe      \
	testCeylanNetwork.exe                      \
	testCeylanSequentialServerStream.exe


testCeylanClientStream_exe_SOURCES            = testCeylanClientStream.cc

testCeylanIPAddressvFour_exe_SOURCES          = testCeylanIPAddressvFour.cc

testCeylanMultiplexedServerStream_exe_SOURCES = \
	testCeylanMultiplexedServerStream.cc
	
testCeylanNetwork_exe_SOURCES                 = testCeylanNetwork.cc

testCeylanSequentialServerStream_exe_SOURCES  = \
	testCeylanSequentialServerStream.cc
