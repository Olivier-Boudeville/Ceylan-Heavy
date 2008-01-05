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

#define CEYLAN_USES_STRERROR

 
// Auto-set the arch flags expected by libnds:
#ifdef CEYLAN_RUNS_ON_ARM7

	#define ARM7
	
	#define CEYLAN_DS_LOG(messageString)
	
#else // CEYLAN_RUNS_ON_ARM7

	#ifdef CEYLAN_RUNS_ON_ARM9

		#define ARM9
	
		// For iprintf and al:
		#include <stdio.h> 

		#define CEYLAN_DS_LOG(messageString) ::iprintf( "[Debug] %s\n", ((messageString).c_str()) )

		// Only included in the DS case:
		#include "CeylanFIFO.h"
		
		
	#else // CEYLAN_RUNS_ON_ARM9

		#error CeylanConfigForNintendoDS.h: either CEYLAN_RUNS_ON_ARM7 or CEYLAN_RUNS_ON_ARM9 must be defined.

	#endif // CEYLAN_RUNS_ON_ARM9

#endif // CEYLAN_RUNS_ON_ARM7


// For iprintf and al:
#include <stdio.h> 

// For libnds (discriminates between ARM7/ARM9):
#include "nds.h"


#endif // CEYLAN_CONFIG_FOR_NINTENDO_DS

