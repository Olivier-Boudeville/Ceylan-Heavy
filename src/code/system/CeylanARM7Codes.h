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
 */ 


/**
 * ARM7 status word values.
 *
 */

/* If no ARM7 status is available (ex: before being set) */
const ARM7StatusWord NoStatusAvailable = 0 ;

/* ARM7 running normally */
const ARM7StatusWord ARM7Running = 1 ;

/* ARM7 in error (see the error code for interpretation) */
const ARM7StatusWord ARM7InError = 2 ;



/**
 * ARM7 error code values.
 *
 */

/* ARM7 has no error registered */
const ARM7ErrorCode NoError = 0 ;

/* ARM7 received an unknown system (Ceylan-specific) command */
const ARM7ErrorCode UnexpectedSystemCommand = 1 ;

/* ARM7 received an unknown user (application-specific) command */
const ARM7ErrorCode UnexpectedApplicationCommand = 2 ;

/* ARM7 detected a FIFO error while reading */
const ARM7ErrorCode FIFOErrorWhileReading = 3 ;

/* ARM7 detected a FIFO error while writing */
const ARM7ErrorCode FIFOErrorWhileWriting = 4 ;



#endif // CEYLAN_ARM7_CODES_H_

