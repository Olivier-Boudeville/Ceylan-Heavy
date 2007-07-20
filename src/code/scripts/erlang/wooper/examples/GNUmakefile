TOP = ../..


.PHONY: 

include $(TOP)/GNUmakerules.inc

HELPER_MODULES = \
	$(COMMON)/utils.beam                    \
	$(COMMON)/hashtable.beam                \
	$(WOOPER_SRC)/wooper_class_manager.beam


all: class_Creature_test class_Mammal_test


class_Creature_test: class_Creature.beam

class_Creature.beam: $(HELPER_MODULES)


class_Mammal_test: class_Mammal.beam class_Creature.beam $(HELPER_MODULES)



