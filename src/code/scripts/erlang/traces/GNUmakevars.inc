# Prerequisite: TRACE_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Trace
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 0.2
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = trace
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(TRACE_TOP)
endif


TRACE_RELEASE_BASE := trace-$(PROJECT_VERSION)

TRACE_RELEASE_ARCHIVE_ZIP := $(TRACE_RELEASE_BASE).zip
TRACE_RELEASE_ARCHIVE_BZ2 := $(TRACE_RELEASE_BASE).tar.bz2


# Source section.
TRACE_SRC  := $(TRACE_TOP)/src

# BEAM path section.
TRACE_BEAM  := $(TRACE_TOP)/src

# Include path section.
TRACE_INC = -I$(TRACE_SRC)


DOC_ROOT      = $(TRACE_TOP)/../../../doc


INC += $(TRACE_INC)



ifndef VM_TEST_NAME

VM_NAME := trace_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := trace_test

endif


BEAM_DIRS += $(TRACE_BEAM)


WOOPER_TOP = $(TRACE_TOP)/../wooper

