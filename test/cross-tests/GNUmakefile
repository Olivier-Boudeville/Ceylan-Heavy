# This is a simple Makefile (not a Makefile.cross, not using overall 
# Makerules.inc rules so that tests are completely uncoupled from the 
# Ceylan library).

# This way, these tests can be directly used as guidelines to build 
# one's application using the Ceylan library.

TOP = ../..


.PHONY: test emulate emulate-with-NoCashGBA emulate-with-DeSmuME transfer \
	patch umount export clean info info-target info-flags info-debug info-run


CROSS_TARGET=nintendo-ds

# For tool locations, not for flags nor build rules:
include $(TOP)/src/conf/build/$(CROSS_TARGET)/Makevars.inc


# Test target section.


# Target linker can be sc (SuperCard) or r4 (R4 DS):
TARGET_LINKER = r4

TEST_PREFIX= $(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)
TEST_EXEC_EXTENSION = $(TARGET_LINKER).nds

TEST_ARM9_SOURCES = $(wildcard *.arm9.cc)


TEST_TARGETS = $(patsubst %.arm9.cc,%-$(TEST_PREFIX)-patched.$(TEST_EXEC_EXTENSION),$(TEST_ARM9_SOURCES))
#TEST_TARGETS = testCeylanFIFO-0.7-release-patched.$(TARGET_LINKER).nds

# Some are patched, the others not:
#TESTS_TO_TRANSFER = $(wildcard *.$(CURRENT_LINKER).nds)

RESOURCES = 

FILES_TO_TRANSFER = $(TEST_TARGETS) $(RESOURCES)


CEYLAN_TESTED_VERSION    = $(CEYLAN_VERSION)
CEYLAN_TESTED_BUILD_MODE = $(CEYLAN_BUILD_MODE)

# Use 'make emulate TEST_NAME=testCeylanLogSystem' for example to override:
TEST_NAME = testCeylanFIFO

TEST_FULL_NAME = $(TEST_NAME)-$(TEST_PREFIX)


all: $(TEST_TARGETS)


test: emulate transfer


emulate: emulate-with-NoCashGBA 


emulate-with-NoCashGBA: $(TEST_FULL_NAME).ds.gba
	@echo "    Running $< thanks to NoCashGBA emulator"
	@$(WINE) $(NO_CASH_GBA) $<
	
	
emulate-with-DeSmuME: $(TEST_FULL_NAME).nds
	@echo "    Running $< thanks to DeSmuME emulator"
	@$(WINE) $(DESMUME) $<


patch: $(TEST_FULL_NAME).sc.nds
	@echo "    Patching $< with script $(DLDITOOL_SCRIPT)"
	@$(DLDITOOL) $(DLDITOOL_SCRIPT) $<
	
	
transfer: $(FILES_TO_TRANSFER)
	@echo "    Transferring $^ to $(TRANSFER_DIRECTORY)"
	@/bin/ls $(TRANSFER_DIRECTORY) 1>/dev/null 2>&1
	@cp -f $^ $(TRANSFER_DIRECTORY)
	

umount: 	
	@echo "    Unmounting $(TRANSFER_MOUNT_POINT)"
	@pumount $(TRANSFER_MOUNT_POINT)


export: transfer umount

	

info-target:
	@echo "TEST_ARM9_SOURCES = $(TEST_ARM9_SOURCES)"
	@echo "TEST_TARGETS = $(TEST_TARGETS)"
	@echo "TARGET_ROM_NAME_BASE = $(TARGET_ROM_NAME_BASE)"
	@echo "CEYLAN_ARM7_INSTALLED_LIB = $(CEYLAN_ARM7_INSTALLED_LIB)"
	@echo "CEYLAN_ARM9_INSTALLED_LIB = $(CEYLAN_ARM9_INSTALLED_LIB)"






# Build flags section.


# Build common section.

TEST_ARCH_FLAGS_COMMON = -mthumb -mthumb-interwork



# Preprocessor section: exports TEST_CPP_FLAGS_ARM7 / TEST_CPP_FLAGS_ARM9

TEST_CPP_FLAGS_COMMON = -I. -I$(CEYLAN_INSTALL_INC) \
	-I$(libfat_DIR)/include -I$(libnds_DIR)/include \
	-DCEYLAN_ARCH_NINTENDO_DS -DCEYLAN_DLL=

TEST_CPP_FLAGS_ARM7 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM7

TEST_CPP_FLAGS_ARM9 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM9



# Compiler section: exports TEST_CXX_FLAGS_ARM7 /TEST_CXX_FLAGS_ARM9


# Long but safe:
# (-Winline removed, caused warning when inlining of test methods failed)
TEST_CXX_FLAGS_COMMON = $(TEST_ARCH_FLAGS_COMMON) -Wall -Werror -Woverloaded-virtual -W -Wfloat-equal -Wundef -Wendif-labels -Wshadow -Wpointer-arith -Wcast-qual -Wwrite-strings -Wconversion -Wsign-compare -Wmissing-format-attribute -Wredundant-decls -Wno-unused-parameter -Wno-unknown-pragmas -Wno-long-long

# -Wconversion had to be removed because of incomprehensible warnings like:
# 'passing argument 1 of XXX with different width due to prototype'
TEST_CC_FLAGS_COMMON = $(TEST_ARCH_FLAGS_COMMON) -Winline -Wall -Werror  -W -Wfloat-equal -Wundef -Wendif-labels -Wshadow -Wpointer-arith -Wcast-qual -Wwrite-strings -Wsign-compare -Wmissing-format-attribute -Wredundant-decls -Wno-unused-parameter -Wno-unknown-pragmas -Wno-long-long


# Tests will follow the release mode of the Ceylan library:
ifeq ($(CEYLAN_TESTED_BUILD_MODE),debug)

# Flags common to the ARM7 and the ARM9 for compilation in debug mode:
TEST_CXX_FLAGS_MODE = -g -O0
TEST_CC_FLAGS_MODE = -g -O0

else

# Flags common to the ARM7 and the ARM9 for compilation in release mode:
TEST_CXX_FLAGS_MODE = -O3 -fomit-frame-pointer -ffast-math
TEST_CC_FLAGS_MODE = -O3 -fomit-frame-pointer -ffast-math

endif


TEST_CXX_FLAGS_ARM7 = $(TEST_CXX_FLAGS_COMMON) $(TEST_CXX_FLAGS_MODE) -mcpu=arm7tdmi -mtune=arm7tdmi 

TEST_CC_FLAGS_ARM7 = $(TEST_CC_FLAGS_COMMON) $(TEST_CC_FLAGS_MODE) -mcpu=arm7tdmi -mtune=arm7tdmi 


TEST_CXX_FLAGS_ARM9 = $(TEST_CXX_FLAGS_COMMON) $(TEST_CXX_FLAGS_MODE) -march=armv5te -mtune=arm946e-s 

TEST_CC_FLAGS_ARM9 = $(TEST_CXX_FLAGS_COMMON) $(TEST_CXX_FLAGS_MODE) -march=armv5te -mtune=arm946e-s 


# Linker section: exports TEST_LD_FLAGS_ARM7 / TEST_LD_FLAGS_ARM9,
# TEST_LIBPATHS_ARM7 / TEST_LIBPATHS_ARM9 and TEST_LIBS_ARM7 / TEST_LIBS_ARM9


# TEST_LD_FLAGS_ARM7 and TEST_LD_FLAGS_ARM9 section:
TEST_LD_FLAGS_COMMON:= $(TEST_ARCH_FLAGS_COMMON)

TEST_LD_FLAGS_ARM7:= -specs=ds_arm7.specs
TEST_LD_FLAGS_ARM9:= -specs=ds_arm9.specs 


ifeq ($(CEYLAN_TESTED_BUILD_MODE),debug)

TEST_LD_FLAGS_COMMON:= $(TEST_LD_FLAGS_COMMON) -g 

else

TEST_LD_FLAGS_COMMON:= $(TEST_LD_FLAGS_COMMON)

endif


TEST_LD_FLAGS_ARM7:= $(TEST_LD_FLAGS_COMMON) $(TEST_LD_FLAGS_ARM7)
TEST_LD_FLAGS_ARM9:= $(TEST_LD_FLAGS_COMMON) $(TEST_LD_FLAGS_ARM9)



# TEST_LIBPATHS_ARM7 and TEST_LIBPATHS_ARM9 section:

# Let the possibility for the user to define his own LIBPATHS:
TEST_LIBPATHS_COMMON = $(LIBPATHS) -L$(libfat_DIR)/lib -L$(libnds_DIR)/lib

TEST_LIBPATHS_ARM7 = -L$(CEYLAN_INSTALL_LIB) $(TEST_LIBPATHS_COMMON) 
TEST_LIBPATHS_ARM9 = -L$(CEYLAN_INSTALL_LIB) $(TEST_LIBPATHS_COMMON) 



# TEST_LIBS_ARM7 and TEST_LIBS_ARM9 section:

# Let the possibility for the user to define his own LIBS.
# Not libfat for the ARM7 yet (library too big):
TEST_BASE_LIBS_ARM7:= $(LIBS) -lnds7
TEST_BASE_LIBS_ARM9:= $(LIBS) -lfat -lnds9


TEST_LIBS_ARM7 = \
       -lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm7 \
       $(TEST_BASE_LIBS_ARM7)
	
TEST_LIBS_ARM9 = \
       -lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm9 \
       $(TEST_BASE_LIBS_ARM9)


	
info: info-target info-flags info-debug info-run


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


info-debug:
	@echo "nm command-line: $(NM) --demangle "
	@echo "strip command-line: $(STRIP)"
		

info-run:
	@echo "NO_CASH_GBA = $(NO_CASH_GBA)"
	@echo "DESMUME = $(DESMUME)"
	

	


# Rules section.



# From source files specific to either ARM to object files:

%-$(TEST_PREFIX).arm7.o: %.arm7.c
	@echo "    Compiling $< into $@"
	$(CC) $(TEST_CC_FLAGS_ARM7) $(TEST_CPP_FLAGS_ARM7) -c $< -o $@

%-$(TEST_PREFIX).arm7.o: %.arm7.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(TEST_CXX_FLAGS_ARM7) $(TEST_CPP_FLAGS_ARM7) -c $< -o $@
	
%.arm7.o: %.arm7.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(TEST_CXX_FLAGS_ARM7) $(TEST_CPP_FLAGS_ARM7) -c $< -o $@
	
	
%-$(TEST_PREFIX).arm9.o: %.arm9.cc
	@echo "    Compiling $< into $@"
	$(CXX) $(TEST_CXX_FLAGS_ARM9) $(TEST_CPP_FLAGS_ARM9) -c $< -o $@


# From object files to fully linked ELF executables:

%.arm7.elf: %.arm7.o $(CEYLAN_ARM7_INSTALLED_LIB)
	@echo "    Creating Ceylan library-user ARM7 ELF executable $@ from $< and from Ceylan ARM7 library"
	@$(CXX) $(TEST_LD_FLAGS_ARM7) -Wl,-Map,$@.map $< $(TEST_LIBPATHS_ARM7) $(TEST_LIBS_ARM7) -o $@


%.arm9.elf: %.arm9.o $(CEYLAN_ARM9_INSTALLED_LIB)
	@echo "    Creating Ceylan library-user ARM9 ELF executable $@ from $< and from Ceylan ARM9 library"
	@$(CXX) $(TEST_LD_FLAGS_ARM9) -Wl,-Map,$@.map $< $(TEST_LIBPATHS_ARM9) $(TEST_LIBS_ARM9)  -o $@




# From ELF executable to DS bin executable format:


%.arm7.bin: %.arm7.elf
	@echo "    Creating ARM7 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@


%.arm9.bin: %.arm9.elf
	@echo "    Creating ARM9 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@



# Neither basic logo, wifi logo nor sentences embedded:
# (sed removes the first dependency, leaving only the ARM9 one)
%.nds: %.arm7.bin %.arm9.bin
	@echo "    Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


# Rules overriden to use FIFO-specific ARM7 executable:
testCeylanFIFO-$(TEST_PREFIX).nds: testCeylanFIFO-$(TEST_PREFIX).arm7.bin \
		testCeylanFIFO-$(TEST_PREFIX).arm9.bin
	@echo "    Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


# Neither basic logo, wifi logo nor sentences embedded:
%.nds: CeylanDefaultMain.arm7.bin %.arm9.bin
	@echo "    Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


%.ds.gba: %.nds
	@echo "    Creating final slot-2 DS ROM $@ from $^"
	@$(DSBUILD) $<


# Patching does not stop on error, as not all tests have to be patched.


# sc for SuperCard linker:

# .sc.nds not working, use .nds instead
#%-patched.sc.nds: %.sc.nds $(CEYLAN_ARM9_INSTALLED_LIB)
#	@echo "    Patching $< into $@ with DLDI patch $(DLDITOOL_SC_SCRIPT)"
#	@$(DLDITOOL) $(DLDITOOL_SC_SCRIPT) $< ; /bin/cp -f $< $@

%-patched.nds: %.nds $(CEYLAN_ARM9_INSTALLED_LIB)
	@echo "    Patching $< into $@ with DLDI patch $(DLDITOOL_SC_SCRIPT)"
	@$(DLDITOOL) $(DLDITOOL_SC_SCRIPT) $< ; /bin/cp -f $< $@


# r4 for R4 linker:
%-patched.r4.nds: %.nds $(CEYLAN_ARM9_INSTALLED_LIB)
	@echo "    Patching $< into $@ with DLDI patch $(DLDITOOL_R4_SCRIPT)"
	@$(DLDITOOL) $(DLDITOOL_R4_SCRIPT) $< ; /bin/cp -f $< $@


# em for FCSR-based ROM for emulators:
%-patched.em.nds: %.ds.gba $(CEYLAN_ARM9_INSTALLED_LIB)
	@echo "    Patching $< into $@ with DLDI patch $(DLDITOOL_FCSR_SCRIPT)"
	@$(DLDITOOL) $(DLDITOOL_FCSR_SCRIPT) $< ; /bin/cp -f $< $@


%.sc.nds: %.ds.gba
	@echo "    Renaming $< into $@ for final slot-2 DS ROM for SuperCard linker"
	@/bin/cp -f $< $@


clean:
	@echo "    Cleaning tests"
	@rm -f *.o *.elf *.map *.bin *.nds *.ds.gba

