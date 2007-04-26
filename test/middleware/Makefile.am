SUFFIXES = .cc .h


install-data-local:
	@echo; echo " Installing playTests-local.sh"
	@mkdir -p $(DESTDIR)@CEYLAN_TEST_DIR@/middleware
	@cp -f $(srcdir)/playTests-local.sh $(DESTDIR)@CEYLAN_TEST_DIR@/middleware


testsmiddlewaredir=@CEYLAN_TEST_DIR@/middleware


testsmiddleware_PROGRAMS = \
	testCeylanLwMarshaller.exe        \
	testCeylanMultiLwMrshSrv.exe      \
	testCeylanMultiLwPtclSrv.exe      \
	testCeylanProtocolClient.exe
	

testCeylanLwMarshaller_exe_SOURCES       = \
	testCeylanLwMarshaller.cc
	
testCeylanMultiLwMrshSrv_exe_SOURCES     = \
	testCeylanMultiLwMrshSrv.cc
	
testCeylanMultiLwPtclSrv_exe_SOURCES     = \
	testCeylanMultiLwPtclSrv.cc

testCeylanProtocolClient_exe_SOURCES     = \
	testCeylanProtocolClient.cc
	
