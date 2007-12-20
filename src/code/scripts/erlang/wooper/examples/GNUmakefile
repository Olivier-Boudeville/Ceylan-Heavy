TOP = ../..


.PHONY: all


all: \
	class_Creature_test           \
	class_Mammal_test             \
	class_Reptile_test            \
	class_ViviparousBeing_test    \
	class_OvoviviparousBeing_test \
	class_Cat_test                \
	class_Platypus_test



include $(TOP)/GNUmakerules.inc

HELPER_MODULES = \
	$(COMMON)/utils.beam                    \
	$(COMMON)/hashtable.beam                \
	$(WOOPER_SRC)/wooper_class_manager.beam


class_Creature_test: class_Creature.beam $(HELPER_MODULES)


class_Mammal_test: class_Mammal.beam class_Creature.beam $(HELPER_MODULES)

class_Reptile_test: class_Reptile.beam class_Creature.beam $(HELPER_MODULES)


class_ViviparousBeing_test: class_ViviparousBeing $(HELPER_MODULES)

class_OvoviviparousBeing_test: class_OvoviviparousBeing $(HELPER_MODULES)


class_Cat_test: class_Mammal_test class_ViviparousBeing_test

class_Platypus_test: class_ViviparousBeing_test class_OvoviviparousBeing_test

