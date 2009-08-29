
# Trace-specific rules:


ERL_FILES = $(wildcard *.erl)


TRACE_LOG_FILES = $(patsubst %.erl,%.traces,$(ERL_FILES))


clean: clean-traces


clean-traces:
	-@rm -f $(TRACE_LOG_FILES)
	
