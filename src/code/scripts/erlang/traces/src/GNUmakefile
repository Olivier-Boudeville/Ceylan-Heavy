TRACE_TOP = ..

	
include $(TRACE_TOP)/GNUmakesettings.inc


# Lists the modules that should be recompiled once TraceType (as defined in
# traces.hrl) is modified:
# (however all other modules including macros like 'test_start' should be
# rebuild as well)
TRACE_TYPE_DEPENDENT_FILES = \
	class_TraceEmitter.beam    \
	class_TraceSupervisor.beam \
	class_TraceAggregator.beam


$(TRACE_TYPE_DEPENDENT_FILES): traces.hrl

