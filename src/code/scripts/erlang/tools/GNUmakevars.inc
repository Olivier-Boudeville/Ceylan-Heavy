# Prerequisite: ERLANG_TOOLS_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
        PROJECT_NAME = Tools
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
        PROJECT_VERSION = 0.1
endif


ifndef PACKAGE_NAME
        PACKAGE_NAME = tools
endif


ifndef PACKAGE_TOP
        PACKAGE_TOP = $(ERLANG_TOOLS_TOP)
endif


TRACES_TOP = $(ERLANG_TOOLS_TOP)/../traces

