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


#ifndef CEYLAN_ARM7_CODES_H_
#define CEYLAN_ARM7_CODES_H_



/*
 * This file only applies to the Nintendo DS.
 *
 */
 
 
#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1



/**
 * Define to 1 to allow for more checkings, for both ARMs.
 * Use safer settings, command count and tests.
 *
 * AS the mechanism has been tested and validated, prefer setting it to zero
 * so that user code can take advantage of one more byte in a FIFO command
 * element.
 *
 */
#define CEYLAN_SAFE_FIFO 0



/*
 * Apparently the overhead due to the C++, to the STL and (marginally) to
 * the full-blown C++ Ceylan library itself leads to having an ARM7 executable
 * too big to fit in its IWRAM.
 *
 * Hence for the moment only a stripped-down C version of the Ceylan library
 * is used.
 *
 */
#define USE_FULL_CEYLAN_FOR_ARM7 0 

#if USE_FULL_CEYLAN_FOR_ARM7


#include "CeylanTypes.h" // for Ceylan::Uint16 and al

typedef Ceylan::Uint16 ARM7StatusWord ;

typedef Ceylan::Uint16 ARM7ErrorCode ;

typedef Ceylan::Uint8 FIFOCommandID ;


#else // USE_FULL_CEYLAN_FOR_ARM7



/**
 * This include file centralizes all the ARM7 status words and error codes.
 *
 * It is to be included by ARM7 C code (for setting) and ARM9 C++ code (for
 * interpretation).
 *
 */ 



	#ifdef CEYLAN_RUNS_ON_ARM9

		/* Cannot be used as require C++: #include "CeylanTypes.h" */

		#define ARM9

	#else // CEYLAN_RUNS_ON_ARM9

		#define ARM7

	#endif // CEYLAN_RUNS_ON_ARM9

#include "nds.h" /* for uint16 and al */


/**
 * Describes an ARM7 status word.
 *
 * @note The definition of their meaning is centralized in 
 * CeylanARM7Codes.h
 *
 */
typedef uint16 ARM7StatusWord ;



/**
 * Describes an ARM7 error word.
 *
 * @note The definition of their meaning is centralized in 
 * CeylanARM7Codes.h
 *
 */
typedef uint16 ARM7ErrorCode ;



/**
 * The identifier of a FIFO command, to be read from the first byte of 
 * its first FIFO element, using the FIFO command mask.
 *
 * @note The definition of their meaning is centralized in 
 * CeylanIPCCommands.h
 *
 */
typedef uint8 FIFOCommandID ;



#endif // USE_FULL_CEYLAN_FOR_ARM7


#endif // defined(CEYLAN_ARCH_NINTENDO_DS)




/**
 * ARM7 status word declarations.
 *
 */


/* This value is left unused, as it may appear in case of technical problem */
extern CEYLAN_DLL const ARM7StatusWord StatusVoluntarilyLeftBlank ;


/* ARM7 running normally */
extern CEYLAN_DLL const ARM7StatusWord ARM7Running ;


/* ARM7 in error (see the error code for interpretation) */
extern CEYLAN_DLL const ARM7StatusWord ARM7InError ;


/* ARM7 IPC shutdown */
extern CEYLAN_DLL const ARM7StatusWord ARM7IPCShutdown ;


/* If no ARM7 status is available (ex: before being set) */
extern CEYLAN_DLL const ARM7StatusWord NoStatusAvailable ;


/* If no ARM7 status variable is available (ex: before being allocated) */
extern CEYLAN_DLL const ARM7StatusWord NoStatusVariableAvailable ;


/* 
 * If no ARM7 status is available due to a status reset (ex: because of
 * read/write operations)
 */
extern CEYLAN_DLL const ARM7StatusWord StatusReset ;




/**
 * ARM7 error code values.
 *
 * Error codes in the [0..1023] range are reserved for Ceylan use.
 *
 */


/* This value is left unused, as it may appear in case of technical problem */
extern CEYLAN_DLL const ARM7ErrorCode ErrorVoluntarilyLeftBlank ;


/* ARM7 received an unknown system (Ceylan-specific) command */
extern CEYLAN_DLL const ARM7ErrorCode UnexpectedSystemCommand ;


/* ARM7 received an unknown external library-specific command */
extern CEYLAN_DLL const ARM7ErrorCode UnexpectedExternalLibraryCommand ;


/* ARM7 received an unknown user (application-specific) command */
extern CEYLAN_DLL const ARM7ErrorCode UnexpectedApplicationCommand ;


/* ARM7 detected a FIFO error while reading */
extern CEYLAN_DLL const ARM7ErrorCode FIFOErrorWhileReading ;


/* ARM7 detected a FIFO error while writing */
extern CEYLAN_DLL const ARM7ErrorCode FIFOErrorWhileWriting ;


/* ARM7 has no error registered */
extern CEYLAN_DLL const ARM7ErrorCode NoError ;


/* If no ARM7 error variable is available (ex: before being allocated) */
extern CEYLAN_DLL const ARM7ErrorCode NoErrorVariableAvailable ;


/* If the ARM7 tried to manage more than one command at a time */
extern CEYLAN_DLL const ARM7ErrorCode CommandOverlapping ;


/* If the ARM7 received an empty (HelloToTheARM7) command */
extern CEYLAN_DLL const ARM7ErrorCode UnexpectedBehaviour ;


/* If the ARM7 received an IPC start command whereas already started */
extern CEYLAN_DLL const ARM7ErrorCode IPCAlreadyStarted ;


/* If the ARM7 received an IPC stop command whereas not running */
extern CEYLAN_DLL const ARM7ErrorCode IPCAlreadyStopped ;


/* If the ARM7 had an IRQ_IPC_SYNC triggered whereas its input FIFO is empty */
extern CEYLAN_DLL const ARM7ErrorCode AwokenWithNothingToRead ;


/* If the ARM7 detected the initial status, set by the ARM9, is incorrect */
extern CEYLAN_DLL const ARM7ErrorCode IncorrectInitialStatus ;


/* If the ARM7 detected the initial error, set by the ARM9, is incorrect */
extern CEYLAN_DLL const ARM7ErrorCode IncorrectInitialError ;


/* If no ARM7 status is available (ex: before being set) */
extern CEYLAN_DLL const ARM7ErrorCode NoErrorAvailable ;


/* If a received command had an unexpected embedded count */
extern CEYLAN_DLL const ARM7ErrorCode UnexpectedCommandCount ;


/* If a received answer to an application command is incorrect (generic) */
extern CEYLAN_DLL const ARM7ErrorCode IncorrectApplicationAnswer ;


/* ARM7 made a time-out while reading */
extern CEYLAN_DLL const ARM7ErrorCode FIFOTimeOutWhileReading ;


/* ARM7 made a time-out while writing */
extern CEYLAN_DLL const ARM7ErrorCode FIFOTimeOutWhileWriting ;



#endif // CEYLAN_ARM7_CODES_H_

