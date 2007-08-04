# This is a simple Makefile (not a Makefile.cross, not using overall 
# Make{vars|rules}.inc files for build settings apart tool locations) so that
# tests are completely uncoupled from the Ceylan library.

# This way, these tests can be directly used as guidelines to build 
# one's application using the Ceylan library.


TOP = ../..

.PHONY:


test: $(TARGET_ROM_NAME_BASE).sc.nds


# Section specific to the Ceylan library being used:
CEYLAN_TESTED_VERSION    := 0.6
CEYLAN_TESTED_BUILD_MODE := release


TARGET_ROM_NAME_BASE = \
	testCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)

 
 
testCeylanForNintendoDS.arm7.elf: testCeylanForNintendoDS.arm7.o \
		$(CEYLAN_ARM7_LIB_FULL_PATH)
	@echo "    Creating Ceylan library-user ARM7 ELF executable $@ from $^"
	@$(CXX) $(LD_FLAGS_ARM7) $< $(CEYLAN_LIBS_ARM7) $(CEYLAN_LIBPATHS_ARM7) -o $@

testCeylanForNintendoDS.arm9.elf: testCeylanForNintendoDS.arm9.o \
		$(CEYLAN_ARM9_LIB_FULL_PATH)
	@echo "    Creating Ceylan library-user ARM9 ELF executable $@ from $^"
	$(CXX) $(LD_FLAGS_ARM9) $< $(CEYLAN_LIBS_ARM9) $(CEYLAN_LIBPATHS_ARM9) -o $@


hellolibuser.arm9.elf: hellolibuser.arm9.o libhellolibrary.arm9.a
	@echo "    Creating library-user ARM9 ELF executable $@ from $<"
	@$(CXX) $(LD_FLAGS_ARM9) $< -lhellolibrary.arm9 -L. $(LIBPATHS_ARM9) $(LIBS_ARM9) -o $@




all: helloworld.sc.nds

test: helloworld.arm7.bin helloworld.arm9.bin 

lib: hellolibrary.arm7.a hellolibrary.arm9.a


# Build common section.

TEST_ARCH_FLAGS_COMMON = -mthumb -mthumb-interwork



# Preprocessor section: exports TEST_CPP_FLAGS_ARM7 / TEST_CPP_FLAGS_ARM9

TEST_CPP_FLAGS_COMMON = -I. -I$(CEYLAN_INSTALL_INC) \
	-I$(libnds_DIR)/include -DCEYLAN_ARCH_NINTENDO_DS

TEST_CPP_FLAGS_ARM7 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM7=1 -DCEYLAN_RUNS_ON_ARM9=0 

TEST_CPP_FLAGS_ARM9 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM7=0 -DCEYLAN_RUNS_ON_ARM9=1



# Compiler section: exports TEST_CXX_FLAGS_ARM7 /TEST_CXX_FLAGS_ARM9


# Long but safe:
TEST_CXX_FLAGS_COMMON = $(TEST_ARCH_FLAGS_COMMON) -Winline -Wall -Werror -Woverloaded-virtual -W -Wfloat-equal -Wundef -Wendif-labels -Wshadow -Wpointer-arith -Wcast-qual -Wwrite-strings -Wconversion -Wsign-compare -Wmissing-format-attribute -Wredundant-decls -Wno-unused-parameter -Wno-unknown-pragmas -Wno-long-long


# Tests will follow the release mode of the Ceylan library:
ifeq ($(CEYLAN_TESTED_BUILD_MODE),debug)

# Flags common to the ARM7 and the ARM9 for compilation in debug mode:
TEST_CXX_FLAGS_MODE = -g -O0

else

# Flags common to the ARM7 and the ARM9 for compilation in release mode:
TEST_CXX_FLAGS_MODE = -O3 -fomit-frame-pointer -ffast-math

endif


TEST_CXX_FLAGS_ARM7 = $(TEST_CXX_FLAGS_COMMON) $(TEST_CXX_FLAGS_MODE) -mcpu=arm7tdmi -mtune=arm7tdmi 

TEST_CXX_FLAGS_ARM9 = $(TEST_CXX_FLAGS_COMMON) $(TEST_CXX_FLAGS_MODE) -march=armv5te -mtune=arm946e-s 



# Linker section: exports TEST_LD_FLAGS_ARM7 / TEST_LD_FLAGS_ARM9,
# TEST_LIBPATHS_ARM7 / TEST_LIBPATHS_ARM9 and TEST_LIBS_ARM7 / TEST_LIBS_ARM9


# TEST_LD_FLAGS_ARM7 and TEST_LD_FLAGS_ARM9 section:
TEST_LD_FLAGS_COMMON:= $(TEST_ARCH_FLAGS_COMMON)

TEST_LD_FLAGS_ARM7:= -specs=ds_arm7.specs -Wl,-Map,ARM7.map
TEST_LD_FLAGS_ARM9:= -specs=ds_arm9.specs -Wl,-Map,ARM9.map

ifeq ($(CEYLAN_TESTED_BUILD_MODE),debug)


TEST_LD_FLAGS_COMMON:= $(TEST_LD_FLAGS_COMMON) -g 

else

TEST_LD_FLAGS_COMMON:= $(TEST_LD_FLAGS_COMMON)

endif

TEST_LD_FLAGS_ARM7:= $(TEST_LD_FLAGS_COMMON) $(TEST_LD_FLAGS_ARM7)
TEST_LD_FLAGS_ARM9:= $(TEST_LD_FLAGS_COMMON) $(TEST_LD_FLAGS_ARM9)



# TEST_LIBPATHS_ARM7 and TEST_LIBPATHS_ARM9 section:

# Let the possibility for the user to define his own LIBPATHS:
TEST_LIBPATHS_COMMON = $(LIBPATHS) -L$(libnds_DIR)/lib

TEST_LIBPATHS_ARM7 = -L$(CEYLAN_INSTALL_LIB) $(TEST_LIBPATHS_COMMON) 
TEST_LIBPATHS_ARM9 = -L$(CEYLAN_INSTALL_LIB) $(TEST_LIBPATHS_COMMON) 



# TEST_LIBS_ARM7 and TEST_LIBS_ARM9 section:

# Let the possibility for the user to define his own LIBS:
TEST_LIBS_ARM7:= $(LIBS) -lnds7
TEST_LIBS_ARM9:= $(LIBS) -lnds9

TEST_LIBS_ARM7:= \
	-lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm7.a \
	$(TEST_LIBS_ARM7)
	
TEST_LIBS_ARM9:= \
	-lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm9.a \
	$(TEST_LIBS_ARM9)




info-flags:
	@echo "TEST_CPP_FLAGS_ARM7 = $(TEST_CPP_FLAGS_ARM7)"
	@echo "TEST_CPP_FLAGS_ARM9 = $(TEST_CPP_FLAGS_ARM9)"
	@echo
	@echo "TEST_CXX_FLAGS_ARM7 = $(TEST_CXX_FLAGS_ARM7)"
	@echo "TEST_CXX_FLAGS_ARM9 = $(TEST_CXX_FLAGS_ARM9)"
	@echo
	@echo "TEST_LD_FLAGS_ARM7 = $(TEST_LD_FLAGS_ARM7)"
	@echo "TEST_LD_FLAGS_ARM9 = $(TEST_LD_FLAGS_ARM9)"
	@echo
	@echo "TEST_LIBPATHS_ARM7 = $(TEST_LIBPATHS_ARM7)"
	@echo "TEST_LIBPATHS_ARM9 = $(TEST_LIBPATHS_ARM9)"
	@echo
	@echo "TEST_LIBS_ARM7 = $(TEST_LIBS_ARM7)"
	@echo "TEST_LIBS_ARM9 = $(TEST_LIBS_ARM9)"


	
# For source files specific to either ARM:

%.arm7.o: %.arm7.cc
	@echo "      Compiling $< into $@"
	$(CXX) $(TEST_CXX_FLAGS_ARM7) $(TEST_CPP_FLAGS_ARM7) -c $< -o $@
	
%.arm9.o: %.arm9.cc
	@echo "      Compiling $< into $@"
	$(CXX) $(TEST_CXX_FLAGS_ARM9) $(TEST_CPP_FLAGS_ARM9) -c $< -o $@


CROSS_TARGET=nintendo-ds


# For tool locations, not for flags nor build rules:
include $(TOP)/src/conf/build/$(CROSS_TARGET)/Makevars.inc
