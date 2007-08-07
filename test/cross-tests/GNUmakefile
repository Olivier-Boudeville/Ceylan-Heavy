# This is a simple Makefile (not a Makefile.cross, not using overall 
# Makerules.inc rules so that tests are completely uncoupled from the 
# Ceylan library.

# This way, these tests can be directly used as guidelines to build 
# one's application using the Ceylan library.

TOP = ../..


.PHONY: test emulate emulate-with-NoCashGBA emulate-with-DeSmuME transfer \
	patch umount export info-target clean info-flags info-debug info-run


CROSS_TARGET=nintendo-ds

# For tool locations, not for flags nor build rules:
include $(TOP)/src/conf/build/$(CROSS_TARGET)/Makevars.inc


# Test target section.

TEST_NAME := testCeylan

CEYLAN_TESTED_VERSION    = $(CEYLAN_VERSION)
CEYLAN_TESTED_BUILD_MODE = $(CEYLAN_BUILD_MODE)

TARGET_ROM_NAME_BASE = \
	$(TEST_NAME)-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)


all: $(TARGET_ROM_NAME_BASE).sc.nds


test: emulate transfer


emulate: emulate-with-NoCashGBA 


emulate-with-NoCashGBA: $(TEST_NAME).ds.gba
	@echo "      Running $< thanks to NoCashGBA emulator"
	@$(WINE) $(NO_CASH_GBA) $<
	
	
emulate-with-DeSmuME: $(TEST_NAME).nds
	@echo "      Running $< thanks to DeSmuME emulator"
	@$(WINE) $(DESMUME) $<


patch: $(TARGET_ROM_NAME_BASE).sc.nds
	@echo "      Patching $< with script $(DLDITOOL_SCRIPT)"
	@$(DLDITOOL) $(DLDITOOL_SCRIPT) $<
	
	
transfer: $(TARGET_ROM_NAME_BASE).sc.nds
	@echo "      Transferring $< to $(TRANSFER_MOUNT_POINT)"
	@cp -f $< $(TRANSFER_MOUNT_POINT)


umount: 	
	@echo "      Unmounting $(TRANSFER_MOUNT_POINT)"
	@pumount $(TRANSFER_MOUNT_POINT)


export: transfer umount

	
$(TARGET_ROM_NAME_BASE).sc.nds: $(TEST_NAME).ds.gba
	@echo "      Renaming $< into $@ for SuperCard linker"
	@cp -f $< $@
	

info-target:
	@echo "TARGET_ROM_NAME_BASE = $(TARGET_ROM_NAME_BASE)"
	@echo "CEYLAN_ARM7_INSTALLED_LIB = $(CEYLAN_ARM7_INSTALLED_LIB)"
	@echo "CEYLAN_ARM9_INSTALLED_LIB = $(CEYLAN_ARM9_INSTALLED_LIB)"






# Build flags section.


# Build common section.

TEST_ARCH_FLAGS_COMMON = -mthumb -mthumb-interwork



# Preprocessor section: exports TEST_CPP_FLAGS_ARM7 / TEST_CPP_FLAGS_ARM9

TEST_CPP_FLAGS_COMMON = -I. -I$(CEYLAN_INSTALL_INC) \
	-I$(libnds_DIR)/include -DCEYLAN_ARCH_NINTENDO_DS

TEST_CPP_FLAGS_ARM7 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM7

TEST_CPP_FLAGS_ARM9 = $(TEST_CPP_FLAGS_COMMON) -DCEYLAN_RUNS_ON_ARM9



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
TEST_BASE_LIBS_ARM7:= $(LIBS) -lnds7
TEST_BASE_LIBS_ARM9:= $(LIBS) -lnds9

TEST_LIBS_ARM7 = \
       -lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm7 \
       $(TEST_BASE_LIBS_ARM7)
	
TEST_LIBS_ARM9 = \
       -lCeylan-$(CEYLAN_TESTED_VERSION)-$(CEYLAN_TESTED_BUILD_MODE)-arm9 \
       $(TEST_BASE_LIBS_ARM9)
	

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

%.arm7.o: %.arm7.cc
	@echo "      Compiling $< into $@"
	@$(CXX) $(TEST_CXX_FLAGS_ARM7) $(TEST_CPP_FLAGS_ARM7) -c $< -o $@
	
	
%.arm9.o: %.arm9.cc
	@echo "      Compiling $< into $@"
	@$(CXX) $(TEST_CXX_FLAGS_ARM9) $(TEST_CPP_FLAGS_ARM9) -c $< -o $@



# From object files to fully linked ELF executables:

%.arm7.elf: %.arm7.o $(CEYLAN_ARM7_INSTALLED_LIB)
	@echo "      Creating Ceylan library-user ARM7 ELF executable $@ from $< and from ARM7 library"
	@$(CXX) $(TEST_LD_FLAGS_ARM7) $< $(TEST_LIBPATHS_ARM7) $(TEST_LIBS_ARM7) -o $@


%.arm9.elf: %.arm9.o $(CEYLAN_ARM9_INSTALLED_LIB)
	@echo "      Creating Ceylan library-user ARM9 ELF executable $@ from $< and from ARM9 library"
	@$(CXX) $(TEST_LD_FLAGS_ARM9) $< $(TEST_LIBPATHS_ARM9) $(TEST_LIBS_ARM9)  -o $@




# From ELF executable to DS bin executable format:


%.arm7.bin: %.arm7.elf
	@echo "      Creating ARM7 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@


%.arm9.bin: %.arm9.elf
	@echo "      Creating ARM9 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@



# Neither basic logo, wifi logo nor sentences embedded:
%.nds: %.arm7.bin %.arm9.bin
	@echo "      Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


# Neither basic logo, wifi logo nor sentences embedded:
%.nds: CeylanDefaultMain.arm7.bin %.arm9.bin
	@echo "      Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


%.ds.gba: %.nds
	@echo "      Creating final slot-2 DS ROM $@ from $^"
	@$(DSBUILD) $<


%.sc.nds: %.ds.gba
	@echo "      Creating final slot-2 DS ROM $@ from $^"
	@/bin/cp -f $< $@


clean:
	@echo "      Cleaning tests"
	@rm -f *.o *.elf *.map *.bin *.nds *.ds.gba

