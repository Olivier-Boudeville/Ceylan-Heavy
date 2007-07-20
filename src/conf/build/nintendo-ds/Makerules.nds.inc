# General build rules for the cross-compilation of Nintendo DS binaries
# from GNU/Linux.
#�To be included by all the Makefiles after they define their relevant TOP
# variable, ex : 'TOP = ..', when the Makefile is in a direct subdirectory 
#�of the source root. 

include $(TOP)/Makevars.$(CROSS_TARGET).inc


.PHONY: info info-paths info-tools info-switches


# Compilation.


# For source files specific to either ARM :

%.arm7.o: %.arm7.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(CXX_FLAGS_ARM7) $(CPP_FLAGS_ARM7) -c $< -o $@
	
%.arm9.o: %.arm9.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(CXX_FLAGS_ARM9) $(CPP_FLAGS_ARM9) -c $< -o $@


# For source files common to both ARM :

%.arm7.o: %.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(CXX_FLAGS_ARM7) $(CPP_FLAGS_ARM7) -c $< -o $@
	
%.arm9.o: %.cc
	@echo "    Compiling $< into $@"
	@$(CXX) $(CXX_FLAGS_ARM9) $(CPP_FLAGS_ARM9) -c $< -o $@



# Executable linking.	


#�Only related object files are included :



# From object files to ELF :

# CXX must be used here, not LD, otherwise :
# arm-eabi-ld: unrecognised emulation mode: thumb-interwork

%.arm7.elf: %.arm7.o
	@echo "    Creating ARM7 ELF executable $@ from $<"
	@$(CXX) $(LD_FLAGS_ARM7) $^ $(LIBPATHS_ARM7) $(LIBS_ARM7) -o $@

%.arm9.elf: %.arm9.o
	@echo "    Creating ARM9 ELF executable $@ from $<"
	@$(CXX) $(LD_FLAGS_ARM9) $^ $(LIBPATHS_ARM9) $(LIBS_ARM9) -o $@


# From ELF to DS bin format :

%.arm7.bin: %.arm7.elf
	@echo "    Creating ARM7 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@

%.arm9.bin: %.arm9.elf
	@echo "    Creating ARM9 bin executable $@ from $<"
	@$(OBJCOPY) -O binary $< $@


# Neither basic logo, wifi logo nor sentences embedded :
%.nds: %.arm7.bin %.arm9.bin
	@echo "    Creating final slot-1 DS ROM $@ from $^"
	@$(NDSTOOL) -c $@ -7 $< -9 `echo $^| sed 's|^.* ||1'`


%.ds.gba: %.nds
	@echo "    Creating final slot-2 DS ROM $@ from $^"
	@$(DSBUILD) $<


%.sc.nds: %.ds.gba
	@echo "    Creating final slot-2 DS ROM $@ from $^"
	@/bin/cp -f $< $@


#�Add here : 
# dlditool Sclt.dldi helloworld.sc.nds


clean:
	@echo "    Cleaning in "$(shell basename `pwd`)
	@-rm -f *.o *.map *.bin *.elf *.nds *.ds.gba 
	

#�Cancel automatic rule :	
%.o: %.cc


info: info-paths info-tools info-switches

info-paths:
	@echo "NDS_DEV_ROOT    = $(NDS_DEV_ROOT)"
	@echo "devkitPro_DIR   = $(devkitPro_DIR)"
	@echo "devkitARM_DIR   = $(devkitARM_DIR)"
	@echo "libnds_DIR      = $(libnds_DIR)"


info-tools:
	@echo "ADDR2LINE       = $(ADDR2LINE)"
	@echo "AR              = $(AR)"
	@echo "AS              = $(AS)"
	@echo "CC              = $(CC)"
	@echo "CXX             = $(CXX)"
	@echo "LD              = $(LD)"
	@echo "NM              = $(NM)"
	@echo "OBJCOPY         = $(OBJCOPY)"
	@echo "OBJDUMP         = $(OBJDUMP)"
	@echo "RANLIB          = $(RANLIB)"
	@echo "STRIP           = $(STRIP)"
	@echo "DLDITOOL        = $(DLDITOOL)"
	@echo "DLDITOOL_SCRIPT = $(DLDITOOL_SCRIPT)"
	@echo "NDSTOOL         = $(NDSTOOL)"


info-switches:
	@echo "build_mode      = $(build_mode)"
	@echo "CXX_FLAGS_ARM7  = $(CXX_FLAGS_ARM7)"
	@echo "CXX_FLAGS_ARM9  = $(CXX_FLAGS_ARM9)"
	@echo "CPP_FLAGS_ARM7  = $(CPP_FLAGS_ARM7)"
	@echo "CPP_FLAGS_ARM9  = $(CPP_FLAGS_ARM9)"
	@echo "LD_FLAGS_ARM7   = $(LD_FLAGS_ARM7)"
	@echo "LD_FLAGS_ARM9   = $(LD_FLAGS_ARM9)"
	@echo "LIBPATHS_ARM7   = $(LIBPATHS_ARM7)"
	@echo "LIBPATHS_ARM9   = $(LIBPATHS_ARM9)"
	@echo "LIBS_ARM7       = $(LIBS_ARM7)"
	@echo "LIBS_ARM9       = $(LIBS_ARM9)"

	