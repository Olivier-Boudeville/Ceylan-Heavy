#ifndef CEYLAN_CONFIG_FOR_NINTENDO_DS
#define CEYLAN_CONFIG_FOR_NINTENDO_DS


/*
 * Common defines for cross-compilation to the Nintendo DS.
 *
 * The library user just has to ensure that either CEYLAN_RUNS_ON_ARM7 or
 * or CEYLAN_RUNS_ON_ARM9 is set to 1, the other one being set to 0.
 *
 * Not related in any way to autoconf.
 *
 */

#define CEYLAN_DLL

// Auto-set the arch flags expected by libnds:
#if CEYLAN_RUNS_ON_ARM7

#define ARM7

#else // CEYLAN_RUNS_ON_ARM7

#if CEYLAN_RUNS_ON_ARM9

#define ARM9

#else // CEYLAN_RUNS_ON_ARM9

#error CeylanConfigForNintendoDS.h: either CEYLAN_RUNS_ON_ARM7 or CEYLAN_RUNS_ON_ARM9 must be set to 1.

#endif // CEYLAN_RUNS_ON_ARM9

#endif // CEYLAN_RUNS_ON_ARM7


// For libnds (discriminates between ARM7/ARM9):
#include "nds.h"


#endif // CEYLAN_CONFIG_FOR_NINTENDO_DS