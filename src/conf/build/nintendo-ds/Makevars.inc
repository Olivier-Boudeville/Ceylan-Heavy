# General variables for the cross-compilation of Nintendo DS binaries
# from GNU/Linux.

# Automatically included by Makerules.nds.inc.

# The NDS_DEV_ROOT variable can be set outside of these Makefiles.
# Either through a shell variable once the local definition has been commented
# out, ex: 
# export NDS_DEV_ROOT=$LOANI_INSTALLATIONS/Nintendo-DS-development
# or as a Make variable (no commenting out needed), ex :
# make NDS_DEV_ROOT=$LOANI_INSTALLATIONS/Nintendo-DS-development
# or directly hardcoded here :
# NDS_DEV_ROOT = $(LOANI_INSTALLATIONS)/Nintendo-DS-development
# Otherwise it will be guessed from LOANI-enforced directory layout :
LOANI_INSTALLATIONS = $(TOP)/../../../../LOANI-installations/
NDS_DEV_ROOT        = $(LOANI_INSTALLATIONS)/Nintendo-DS-development


# Safety check :
ifeq ($(strip $(NDS_DEV_ROOT)),)
$(error Please set the NDS_DEV_ROOT variable, see Makevars.nds.inc for hints)
endif



devkitPro_DIR = $(NDS_DEV_ROOT)/devkitPro
devkitARM_DIR = $(devkitPro_DIR)/devkitARM
libnds_DIR    = $(devkitPro_DIR)/libnds


build_tool_DIR    = ${devkitARM_DIR}/bin
build_tool_PREFIX =arm-eabi-

ADDR2LINE = $(build_tool_DIR)/$(build_tool_PREFIX)addr2line
AR        = $(build_tool_DIR)/$(build_tool_PREFIX)ar
AS        = $(build_tool_DIR)/$(build_tool_PREFIX)as
CC        = $(build_tool_DIR)/$(build_tool_PREFIX)gcc
CXX       = $(build_tool_DIR)/$(build_tool_PREFIX)g++
LD        = $(build_tool_DIR)/$(build_tool_PREFIX)ld
NM        = $(build_tool_DIR)/$(build_tool_PREFIX)nm
OBJCOPY   = $(build_tool_DIR)/$(build_tool_PREFIX)objcopy
OBJDUMP   = $(build_tool_DIR)/$(build_tool_PREFIX)objdump
RANLIB    = $(build_tool_DIR)/$(build_tool_PREFIX)ranlib
STRIP     = $(build_tool_DIR)/$(build_tool_PREFIX)strip


# The version installed by LOANI is preferred here :
DLDITOOL_ROOT   = $(NDS_DEV_ROOT)/dldi
DLDITOOL        = $(DLDITOOL_ROOT)/dlditool

# Replace by your linker-specific script if not using SuperCard Lite :
DLDITOOL_SCRIPT = $(DLDITOOL_ROOT)/Sclt.dldi


NDSTOOL = $(build_tool_DIR)/ndstool
DSBUILD = $(build_tool_DIR)/dsbuild


# Module settings.

CEYLAN_LOGS        := $(TOP)/src/code/logs
CEYLAN_INTERFACES  := $(TOP)/src/code/interfaces
CEYLAN_SYSTEM      := $(TOP)/src/code/system
CEYLAN_MODULES     := $(TOP)/src/code/modules
CEYLAN_GENERIC     := $(TOP)/src/code/generic
CEYLAN_MATHS       := $(TOP)/src/code/maths
CEYLAN_NETWORK     := $(TOP)/src/code/network
CEYLAN_MIDDLEWARE  := $(TOP)/src/code/middleware


CEYLAN_LOGS_INC        := -I$(CEYLAN_LOGS)
CEYLAN_INTERFACES_INC  := -I$(CEYLAN_INTERFACES) 
CEYLAN_SYSTEM_INC      := -I$(CEYLAN_SYSTEM) 
CEYLAN_MODULES_INC     := -I$(CEYLAN_MODULES) 
CEYLAN_GENERIC_INC     := -I$(CEYLAN_GENERIC) 
CEYLAN_MATHS_INC       := -I$(CEYLAN_MATHS) 
CEYLAN_NETWORK_INC     := -I$(CEYLAN_NETWORK) 
CEYLAN_MIDDLEWARE_INC  := -I$(CEYLAN_MIDDLEWARE) 


# Architecture-related flags.

ARCH_FLAGS_COMMON = -mthumb -mthumb-interwork

# The -DARM7/-DARM flags are needed by libnds :
ARCH_FLAGS_ARM7   = $(ARCH_FLAGS_COMMON) -mcpu=arm7tdmi -mtune=arm7tdmi -DCEYLAN_RUNS_ON_ARM7 -DARM7

ARCH_FLAGS_ARM9   = $(ARCH_FLAGS_COMMON) -march=armv5te -mtune=arm946e-s -DCEYLAN_RUNS_ON_ARM9 -DARM9


# Determines which kind of build is expected :
build_mode = debug
#build_mode = release


# Preprocessor flags.

CPP_FLAGS_COMMON := -DCEYLAN_DLL= $(CPP_FLAGS_COMMON) -I$(libnds_DIR)/include 

# Let the possibility for the user to define his own CPP_FLAGS :
CPP_FLAGS_ARM7   := $(CPP_FLAGS) $(CPP_FLAGS_COMMON)
CPP_FLAGS_ARM9   := $(CPP_FLAGS) $(CPP_FLAGS_COMMON)



# C++ Compile flags.

# Compile flags always used, for both ARM, both in debug and release modes :
CXX_FLAGS_COMMON := -Winline -Wall


ifeq ($(build_mode),debug)

# Flags common to the ARM7 and the ARM9 for compilation in debug mode :
CXX_FLAGS_COMMON := $(CXX_FLAGS_COMMON) -g -O0 -DCEYLAN_DEBUG


else

# Flags common to the ARM7 and the ARM9 for compilation in release mode :
CXX_FLAGS_COMMON := $(CXX_FLAGS_COMMON) -O3 -fomit-frame-pointer -ffast-math

endif

# Let the possibility for the user to define his own CXX_FLAGS :
CXX_FLAGS_ARM7   := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM7)
CXX_FLAGS_ARM9   := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM9)




# C++ Link flags.


# Generating a library :

AR_FLAGS_COMMON := -rc

AR_FLAGS_ARM7 := $(AR_FLAGS_COMMON)
AR_FLAGS_ARM9 := $(AR_FLAGS_COMMON)


# Generating an executable :

LD_FLAGS_COMMON := $(ARCH_FLAGS_COMMON)

LD_FLAGS_ARM7 := -specs=ds_arm7.specs -Wl,-Map,ARM7.map
LD_FLAGS_ARM9 := -specs=ds_arm9.specs -Wl,-Map,ARM9.map


ifeq ($(build_mode),debug)


LD_FLAGS_COMMON := $(LD_FLAGS_COMMON) -g 

else

LD_FLAGS_COMMON := $(LD_FLAGS_COMMON)

endif

# Let the possibility for the user to define his own LD_FLAGS :
LD_FLAGS_ARM7 := $(LD_FLAGS) $(LD_FLAGS_COMMON) $(LD_FLAGS_ARM7)
LD_FLAGS_ARM9 := $(LD_FLAGS) $(LD_FLAGS_COMMON) $(LD_FLAGS_ARM9)


LIBPATHS_COMMON = -L$(libnds_DIR)/lib

# Let the possibility for the user to define his own LIBPATHS :
LIBPATHS_ARM7 := $(LIBPATHS) $(LIBPATHS_COMMON) $(LIBPATHS_ARM7)
LIBPATHS_ARM9 := $(LIBPATHS) $(LIBPATHS_COMMON) $(LIBPATHS_ARM9)

# Let the possibility for the user to define his own LIBS :
LIBS_ARM7 := $(LIBS) -lnds7
LIBS_ARM9 := $(LIBS) -lnds9

 
 
