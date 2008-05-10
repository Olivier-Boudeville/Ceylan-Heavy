TOP = ../..



include $(TOP)/GNUmakerules.inc


# Note: utils and hashtable will be recompiled in place (here) if not already
# available in their expected directory.
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

