/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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

		// Only included in the DS ARM9 case:
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

