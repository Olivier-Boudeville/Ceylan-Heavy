# General variables for the cross-compilation of Nintendo DS binaries
# from GNU/Linux.

# Automatically included by Makerules.nds.inc.

# The NDS_DEV_ROOT variable can be set outside of these Makefiles.
# Either through a shell variable once the local definition has been commented
# out, ex: 
# export NDS_DEV_ROOT=$LOANI_INSTALLATIONS/Nintendo-DS-development
# or as a Make variable (no commenting out needed), ex:
# make NDS_DEV_ROOT=$LOANI_INSTALLATIONS/Nintendo-DS-development
# or directly hardcoded here:
# NDS_DEV_ROOT = $(LOANI_INSTALLATIONS)/Nintendo-DS-development
# Otherwise it will be guessed from LOANI-enforced directory layout:
LOANI_INSTALLATIONS = $(TOP)/../../../../LOANI-installations
NDS_DEV_ROOT        = $(LOANI_INSTALLATIONS)/Nintendo-DS-development


# Safety check:
ifeq ($(strip $(NDS_DEV_ROOT)),)
$(error Please set the NDS_DEV_ROOT variable)
endif


# Tool section.

devkitPro_DIR = $(NDS_DEV_ROOT)/devkitPro
devkitARM_DIR = $(devkitPro_DIR)/devkitARM
libnds_DIR    = $(devkitPro_DIR)/libnds
libfat_DIR    = $(libnds_DIR)


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


# The version installed by LOANI is preferred here:
DLDITOOL_ROOT   = $(NDS_DEV_ROOT)/dldi
DLDITOOL        = $(DLDITOOL_ROOT)/dlditool

# Replace by your linker-specific script if not using SuperCard Lite:
DLDITOOL_SCRIPT = $(DLDITOOL_SC_SCRIPT)

DLDITOOL_SC_SCRIPT   = $(DLDITOOL_ROOT)/Sclt.dldi
DLDITOOL_R4_SCRIPT   = $(DLDITOOL_ROOT)/R4tf.dldi
DLDITOOL_FCSR_SCRIPT = $(DLDITOOL_ROOT)/fcsr.dldi



NDSTOOL = $(build_tool_DIR)/ndstool
DSBUILD = $(build_tool_DIR)/dsbuild

NO_CASH_GBA = $(NDS_DEV_ROOT)/NoCashGBA-w/NoCashGBA.EXE
DESMUME     = $(NDS_DEV_ROOT)/DeSmuME-0.4.0/NDeSmuME.exe

WINE = wine

# Location of the removable media where ROM can be transferred:
#TRANSFER_MOUNT_POINT = /media/disk
TRANSFER_MOUNT_POINT = /media/DS-homebrew

TRANSFER_DIRECTORY= $(TRANSFER_MOUNT_POINT)/NDS



# Module settings.


CEYLAN_CODE_ROOT := $(TOP)/src/code

CEYLAN_LOGS       := $(CEYLAN_CODE_ROOT)/logs
CEYLAN_INTERFACES := $(CEYLAN_CODE_ROOT)/interfaces
CEYLAN_SYSTEM     := $(CEYLAN_CODE_ROOT)/system
CEYLAN_MODULES    := $(CEYLAN_CODE_ROOT)/modules
CEYLAN_GENERIC    := $(CEYLAN_CODE_ROOT)/generic
CEYLAN_MATHS      := $(CEYLAN_CODE_ROOT)/maths
CEYLAN_NETWORK    := $(CEYLAN_CODE_ROOT)/network
CEYLAN_MIDDLEWARE := $(CEYLAN_CODE_ROOT)/middleware


CEYLAN_CODE_ROOT_INC := -I$(CEYLAN_CODE_ROOT)

CEYLAN_LOGS_INC       := -I$(CEYLAN_LOGS)
CEYLAN_INTERFACES_INC := -I$(CEYLAN_INTERFACES) 
CEYLAN_SYSTEM_INC     := -I$(CEYLAN_SYSTEM) 
CEYLAN_MODULES_INC    := -I$(CEYLAN_MODULES) 
CEYLAN_GENERIC_INC    := -I$(CEYLAN_GENERIC) 
CEYLAN_MATHS_INC      := -I$(CEYLAN_MATHS) 
CEYLAN_NETWORK_INC    := -I$(CEYLAN_NETWORK) 
CEYLAN_MIDDLEWARE_INC := -I$(CEYLAN_MIDDLEWARE) 


# For Ceylan versions and al:
include $(TOP)/src/conf/CeylanSettings.inc

CEYLAN_LT_CURRENT := $(CEYLAN_MAJOR_VERSION)
CEYLAN_LT_REVISION:= $(CEYLAN_MINOR_VERSION)
CEYLAN_LT_AGE     := $(shell expr $(CEYLAN_MAJOR_VERSION) - $(CEYLAN_MAJOR_ANCESTOR))

CEYLAN_VERSION:= $(CEYLAN_LT_CURRENT).$(CEYLAN_LT_REVISION)
CEYLAN_LT_VERSION:= $(CEYLAN_LT_CURRENT).$(CEYLAN_LT_REVISION).$(CEYLAN_LT_AGE)


# Architecture-related flags.

# By default prefer THUMB mode (16 bit) to ARM mode (32 bit):
ARCH_FLAGS_COMMON = -mthumb -mthumb-interwork

# To force ARM mode (ex: on the ARM7 for the Helix decoder):
ARCH_FLAGS_COMMON_32_BIT = 


# The -DARM7/-DARM flags are needed by libnds but set by Ceylan DS header:

ARCH_FLAGS_ARM7   = $(ARCH_FLAGS_COMMON) -mcpu=arm7tdmi -mtune=arm7tdmi -DCEYLAN_RUNS_ON_ARM7=1

ARCH_FLAGS_ARM7_32_BIT = $(ARCH_FLAGS_COMMON_32_BIT) -mcpu=arm7tdmi -mtune=arm7tdmi -DCEYLAN_RUNS_ON_ARM7=1


ARCH_FLAGS_ARM9   = $(ARCH_FLAGS_COMMON) -march=armv5te -mtune=arm946e-s -DCEYLAN_RUNS_ON_ARM9=1

ARCH_FLAGS_ARM9_32_BIT   = $(ARCH_FLAGS_COMMON_32_BIT) -march=armv5te -mtune=arm946e-s -DCEYLAN_RUNS_ON_ARM9=1


# Determines which kind of build is expected:
#CEYLAN_BUILD_MODE = debug
CEYLAN_BUILD_MODE = release


# Ceylan library names.

CEYLAN_BASE_LIB_NAME:= Ceylan-$(CEYLAN_VERSION)

ifeq ($(CEYLAN_BUILD_MODE),debug)
	CEYLAN_BASE_LIB_NAME:= $(CEYLAN_BASE_LIB_NAME)-debug
else
	CEYLAN_BASE_LIB_NAME:= $(CEYLAN_BASE_LIB_NAME)-release
endif

CEYLAN_ARM7_LINK_NAME:= $(CEYLAN_BASE_LIB_NAME)-arm7
CEYLAN_ARM9_LINK_NAME:= $(CEYLAN_BASE_LIB_NAME)-arm9


CEYLAN_ARM7_LIB_NAME:= lib$(CEYLAN_ARM7_LINK_NAME).a
CEYLAN_ARM9_LIB_NAME:= lib$(CEYLAN_ARM9_LINK_NAME).a

CEYLAN_LIB_LOCATION:= $(TOP)/src/code

CEYLAN_ARM7_LIB_FULL_PATH:=$(CEYLAN_LIB_LOCATION)/$(CEYLAN_ARM7_LIB_NAME)
CEYLAN_ARM9_LIB_FULL_PATH:=$(CEYLAN_LIB_LOCATION)/$(CEYLAN_ARM9_LIB_NAME)


# Preprocessor flags.

CPP_DEFINES = \
	$(CPP_ARCH_DEFINES)    \
	$(CPP_FEATURE_DEFINES) \
	$(CPP_DEBUG_DEFINES)   \
	$(CPP_VERBOSE_DEFINES) \
	$(CPP_OTHER_DEFINES)
	

CPP_ARCH_DEFINES:= \
	-DCEYLAN_ARCH_NINTENDO_DS=1      \
	-DCEYLAN_ARCH_WINDOWS=0          \
	-DCEYLAN_ARCH_MACOSX=0           \
	-DCEYLAN_RUNS_ON_LITTLE_ENDIAN=1 


CPP_FEATURE_DEFINES:= \
	-DCEYLAN_USES_REGEX=0                        \
	-DCEYLAN_USES_THREADS=0                      \
	-DCEYLAN_USES_NETWORK=0                      \
	-DCEYLAN_USES_FILE_DESCRIPTORS=1             \
	-DCEYLAN_USES_SYMBOLIC_LINKS=0               \
	-DCEYLAN_USES_ADVANCED_FILE_ATTRIBUTES=0     \
	-DCEYLAN_USES_FILE_LOCKS=0                   \
	-DCEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT=0  \
	-DCEYLAN_USES_PLUGINS=0                      \
	-DCEYLAN_USES_SIGNALS=0                      \
	
		
CPP_DEBUG_DEFINES:= \
	-DCEYLAN_DEBUG_CONSOLE=0           \
	-DCEYLAN_DEBUG_DEMANGLE=0          \
	-DCEYLAN_DEBUG_EVENTS=0            \
	-DCEYLAN_DEBUG_FIFO=0              \
	-DCEYLAN_DEBUG_LIBFAT=0            \
	-DCEYLAN_DEBUG_LOG=0               \
	-DCEYLAN_DEBUG_LOW_LEVEL_STREAMS=0 \
	-DCEYLAN_DEBUG_MARSHALLERS=0       \
	-DCEYLAN_DEBUG_NETWORK_CLIENTS=0   \
	-DCEYLAN_DEBUG_NETWORK_SERVERS=0   \
	-DCEYLAN_DEBUG_NINTENDO_DS=1       \
	-DCEYLAN_DEBUG_RANDOM=0            \
	-DCEYLAN_DEBUG_SYSTEM=0            \
	-DCEYLAN_DEBUG_TEXTBUFFER=0        \
	-DCEYLAN_DEBUG_THREADS=0
	
	
CPP_VERBOSE_DEFINES	:=
	
	
CPP_OTHER_DEFINES:= \
	-DCEYLAN_DLL= \
	-DCEYLAN_LIBTOOL_VERSION="\"$(CEYLAN_LT_VERSION)\""


CPP_FLAGS_COMMON:= $(CPP_DEFINES) -I. $(CPP_FLAGS_COMMON) \
	-I$(libfat_DIR)/include -I$(libnds_DIR)/include  


# Let the possibility for the user to define his own CPP_FLAGS:
CPP_FLAGS_ARM7  := $(CPP_FLAGS) $(CPP_FLAGS_COMMON)
CPP_FLAGS_ARM9  := $(CPP_FLAGS) $(CPP_FLAGS_COMMON)


# ARM assembler flags.
ASM_FLAGS = -g -marm -mthumb-interwork


# C and C++ Compile flags.

# Compile flags always used, for both ARM, both in debug and release modes:

# @fixme When needing to gain some room in ARM7 binary, using -Os instead of -O3
# implies removing -Winline:
CC_FLAGS_COMMON:=  -Wall -Werror -W -Wfloat-equal -Wundef -Wendif-labels -Wshadow -Wpointer-arith -Wwrite-strings -Wsign-compare -Wmissing-format-attribute -Wredundant-decls -Wno-unused-parameter -Wno-unknown-pragmas -Wno-long-long

# Removed: 
#   -Wcast-qual: cause Helix code to issue warnings (cast discards qualifiers) 
#   -Wconversion: cause Helix code to issue warnings (prototype/memmove)
#   -Wcast-align: too many warnings due to STL casts
#   -Wpacked: too many warnings about 'packed attribute causes inefficient
# alignment' in libnds headers


CXX_FLAGS_COMMON:= -Winline -Wall -Werror -Woverloaded-virtual -W -Wfloat-equal -Wundef -Wendif-labels -Wshadow -Wpointer-arith -Wcast-qual -Wwrite-strings -Wconversion -Wsign-compare -Wmissing-format-attribute -Wredundant-decls -Wno-unused-parameter -Wno-unknown-pragmas -Wno-long-long

# Removed: 
#   -Wcast-align: too many warnings due to STL casts
#   -Wpacked: too many warnings about 'packed attribute causes inefficient
# alignment' in libnds headers


ifeq ($(CEYLAN_BUILD_MODE),debug)

# Flags common to the ARM7 and the ARM9 for compilation in debug mode:
CC_FLAGS_COMMON:= $(CC_FLAGS_COMMON) -g -O0 -DCEYLAN_DEBUG=1

# Flags common to the ARM7 and the ARM9 for compilation in debug mode:
CXX_FLAGS_COMMON:= $(CXX_FLAGS_COMMON) -g -O0 -DCEYLAN_DEBUG=1

else

# Flags common to the ARM7 and the ARM9 for compilation in release mode:
CC_FLAGS_COMMON:= $(CC_FLAGS_COMMON) -O3 -fomit-frame-pointer -ffast-math -DCEYLAN_DEBUG=0

CXX_FLAGS_COMMON:= $(CXX_FLAGS_COMMON) -O3 -fomit-frame-pointer -ffast-math -DCEYLAN_DEBUG=0

endif


# Let the possibility for the user to define his own CC_FLAGS:

CC_FLAGS_ARM7        := $(CC_FLAGS) $(CC_FLAGS_COMMON) $(ARCH_FLAGS_ARM7)

CC_FLAGS_ARM7_32_BIT := $(CC_FLAGS) $(CC_FLAGS_COMMON) $(ARCH_FLAGS_ARM7_32_BIT)

CC_FLAGS_ARM9        := $(CC_FLAGS) $(CC_FLAGS_COMMON) $(ARCH_FLAGS_ARM9)
CC_FLAGS_ARM9_32_BIT := $(CC_FLAGS) $(CC_FLAGS_COMMON) $(ARCH_FLAGS_ARM9_32_BIT)


# Let the possibility for the user to define his own CXX_FLAGS:

CXX_FLAGS_ARM7        := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM7)
CXX_FLAGS_ARM7_32_BIT := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM7_32_BIT)

CXX_FLAGS_ARM9        := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM9)
CXX_FLAGS_ARM9_32_BIT := $(CXX_FLAGS) $(CXX_FLAGS_COMMON) $(ARCH_FLAGS_ARM9_32_BIT)




# C/C++ Link flags.


# Generating a library:

AR_FLAGS_COMMON:= -rc

AR_FLAGS_ARM7:= $(AR_FLAGS_COMMON)
AR_FLAGS_ARM9:= $(AR_FLAGS_COMMON)


# Generating an executable is defined in trunk/test/cross-tests/GNUmakefile.



# Install section.

CEYLAN_INSTALL_ROOT := $(NDS_DEV_ROOT)/Ceylan-$(CEYLAN_VERSION)
CEYLAN_INSTALL_INC  := $(CEYLAN_INSTALL_ROOT)/include
CEYLAN_INSTALL_LIB  := $(CEYLAN_INSTALL_ROOT)/lib

CEYLAN_ARM7_INSTALLED_LIB := $(CEYLAN_INSTALL_LIB)/$(CEYLAN_ARM7_LIB_NAME)
CEYLAN_ARM9_INSTALLED_LIB := $(CEYLAN_INSTALL_LIB)/$(CEYLAN_ARM9_LIB_NAME)

