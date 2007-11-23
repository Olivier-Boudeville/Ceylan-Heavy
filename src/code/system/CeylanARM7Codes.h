#ifndef CEYLAN_ARM7_CODES_H_
#define CEYLAN_ARM7_CODES_H_


/**
 * This include file centralizes all the ARM7 status words and error codes.
 *
 * It is to be included by ARM7 C code (for setting) and ARM9 C++ code (for
 * interpretation).
 *
 * typedef for ARM7StatusWord and ARM7ErrorCode are expected to be already
 * defined (they cannot be defined here as their definition is specific to
 * each ARM: Ceylan::Uint16/uint16).
 *
 * In the ARM9 this is done automatically thanks to:
 *
 */ 

#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1


/**
 * Define to 1 to allow for more checkings, for both ARMs.
 * Use safer settings, command count and tests.
 *
 */
#define CEYLAN_SAFE_FIFO 1



#ifdef CEYLAN_RUNS_ON_ARM9

#include "CeylanFIFO.h" // for ARM7StatusWord and ARM7ErrorCode typedefs

#else // CEYLAN_RUNS_ON_ARM9

// libnds header and ARM7 define supposed also taken care of.

/* Defines the ARM7 status word type */
typedef uint16 ARM7StatusWord ;

/* Defines the ARM7 error code type */
typedef uint16 ARM7ErrorCode ;

/* Defines the FIFO command identifier type for the ARM7 */
typedef uint8 FIFOCommandID ;


#endif // CEYLAN_RUNS_ON_ARM9


#endif // defined(CEYLAN_ARCH_NINTENDO_DS)



/**
 * ARM7 status word values.
 *
 */


/* This value is left unused, as it may appear in case of technical problem */
const ARM7StatusWord StatusVoluntarilyLeftBlank = 0 ;

/* ARM7 running normally */
const ARM7StatusWord ARM7Running = 1 ;

/* ARM7 in error (see the error code for interpretation) */
const ARM7StatusWord ARM7InError = 2 ;

/* ARM7 IPC shutdown */
const ARM7StatusWord ARM7IPCShutdown = 3 ;

/* If no ARM7 status is available (ex: before being set) */
const ARM7StatusWord NoStatusAvailable = 4 ;

/* If no ARM7 status variable is available (ex: before being allocated) */
const ARM7StatusWord NoStatusVariableAvailable = 5 ;

/* 
 * If no ARM7 status is available due to a status reset (ex: because of
 * read/write operations)
 */
const ARM7StatusWord StatusReset = 6 ;




/**
 * ARM7 error code values.
 *
 * Error codes in the [0..1023] range are reserved for Ceylan use.
 *
 */

/* This value is left unused, as it may appear in case of technical problem */
const ARM7ErrorCode ErrorVoluntarilyLeftBlank = 0 ;

/* ARM7 received an unknown system (Ceylan-specific) command */
const ARM7ErrorCode UnexpectedSystemCommand = 1 ;

/* ARM7 received an unknown user (application-specific) command */
const ARM7ErrorCode UnexpectedApplicationCommand = 2 ;

/* ARM7 detected a FIFO error while reading */
const ARM7ErrorCode FIFOErrorWhileReading = 3 ;

/* ARM7 detected a FIFO error while writing */
const ARM7ErrorCode FIFOErrorWhileWriting = 4 ;

/* ARM7 has no error registered */
const ARM7ErrorCode NoError = 5 ;

/* If no ARM7 error variable is available (ex: before being allocated) */
const ARM7ErrorCode NoErrorVariableAvailable = 6 ;

/* If the ARM7 tried to manage more than one command at a time */
const ARM7ErrorCode CommandOverlapping = 7 ;

/* If the ARM7 received an empty (HelloToTheARM7) command */
const ARM7ErrorCode UnexpectedBehaviour = 8 ;

/* If the ARM7 received an IPC start command whereas already started */
const ARM7ErrorCode IPCAlreadyStarted = 9 ;

/* If the ARM7 received an IPC stop command whereas not running */
const ARM7ErrorCode IPCAlreadyStopped = 10 ;

/* If the ARM7 had an IRQ_IPC_SYNC triggered whereas its input FIFO is empty */
const ARM7ErrorCode AwokenWithNothingToRead = 11 ;

/* If the ARM7 detected the initial status, set by the ARM9, is incorrect */
const ARM7ErrorCode IncorrectInitialStatus = 12 ;

/* If the ARM7 detected the initial error, set by the ARM9, is incorrect */
const ARM7ErrorCode IncorrectInitialError = 13 ;

/* If no ARM7 status is available (ex: before being set) */
const ARM7ErrorCode NoErrorAvailable = 14 ;

/* If a received command had an unexpected embedded count */
const ARM7ErrorCode UnexpectedCommandCount = 15 ;

/* If a received answer to an application command is incorrect (generic) */
const ARM7ErrorCode IncorrectApplicationAnswer = 16 ;

/* ARM7 made a time-out while reading */
const ARM7ErrorCode FIFOTimeOutWhileReading = 17 ;

/* ARM7 made a time-out while writing */
const ARM7ErrorCode FIFOTimeOutWhileWriting = 18 ;

#endif // CEYLAN_ARM7_CODES_H_

