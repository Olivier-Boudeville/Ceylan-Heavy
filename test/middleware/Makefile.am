SUFFIXES = .cc .h


install-data-local:
	@echo; echo " Installing playTests-local.sh"
	@mkdir -p $(DESTDIR)@CEYLAN_TEST_DIR@/middleware
	@cp -f $(srcdir)/playTests-local.sh $(DESTDIR)@CEYLAN_TEST_DIR@/middleware


testsmiddlewaredir=@CEYLAN_TEST_DIR@/middleware


testsmiddleware_PROGRAMS = \
	testCeylanLightWeightMarshaller.exe        \
	testCeylanMultiLwMarshalledServer.exe      \
	testCeylanMultiLwProtocolServer.exe        \
	testCeylanProtocolClient.exe
	

testCeylanLightWeightMarshaller_exe_SOURCES       = \
	testCeylanLightWeightMarshaller.cc
	
testCeylanMultiLwMarshalledServer_exe_SOURCES     = \
	testCeylanMultiLwMarshalledServer.cc
	
testCeylanMultiLwProtocolServer_exe_SOURCES       = \
	testCeylanMultiLwProtocolServer.cc

testCeylanProtocolClient_exe_SOURCES              = \
	testCeylanProtocolClient.cc
	
