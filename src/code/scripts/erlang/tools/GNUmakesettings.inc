
# This is the file that all Erlang Tools GNUmakefiles, and only them,
# should include.

# Prerequisite: ERLANG_TOOLS_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# common/GNUmakesettings.inc.


# Nothing simpler can be used:

#include $(ERLANG_TOOLS_TOP)/GNUmakevars.inc
#WOOPER_TOP = $(ERLANG_TOOLS_TOP)/../wooper
COMMON_TOP = $(ERLANG_TOOLS_TOP)/../common

#include $(WOOPER_TOP)/GNUmakevars.inc
include $(COMMON_TOP)/GNUmakevars.inc

#include $(ERLANG_TOOLS_TOP)/GNUmakerules.inc
#include $(WOOPER_TOP)/GNUmakerules.inc
include $(COMMON_TOP)/GNUmakerules.inc

