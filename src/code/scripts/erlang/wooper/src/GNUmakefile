TOP = ../..


.PHONY: all

all: wooper_class_manager_test



include $(TOP)/GNUmakerules.inc

HELPER_MODULES = \
	$(COMMON)/utils.beam                    \
	$(COMMON)/hashtable.beam                \
	$(WOOPER_SRC)/wooper_class_manager.beam


wooper_class_manager_test: \
	wooper_class_manager_test.beam \
	wooper_class_manager.beam      \
	$(HELPER_MODULES)
