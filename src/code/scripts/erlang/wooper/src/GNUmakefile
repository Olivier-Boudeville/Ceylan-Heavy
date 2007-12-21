TOP = ../..


.PHONY: all tests

all: tests


tests: wooper_class_manager_test class_Wooper_attribute_test



include $(TOP)/GNUmakerules.inc

HELPER_MODULES = \
	$(COMMON)/utils.beam                    \
	$(COMMON)/hashtable.beam                \
	$(WOOPER_SRC)/wooper_class_manager.beam


wooper_class_manager_test: \
	wooper_class_manager_test.beam   \
	wooper_class_manager.beam        \
	$(HELPER_MODULES)


class_Wooper_attribute_test: \
	class_Wooper_attribute_test.beam \
	wooper_class_manager.beam        \
	$(HELPER_MODULES)
