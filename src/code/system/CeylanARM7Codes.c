#include "CeylanARM7Codes.h"



/**
 * ARM7 status word definitions.
 *
 */


const ARM7StatusWord StatusVoluntarilyLeftBlank  = 0 ;
const ARM7StatusWord ARM7Running                 = 1 ;
const ARM7StatusWord ARM7InError                 = 2 ;
const ARM7StatusWord ARM7IPCShutdown             = 3 ;
const ARM7StatusWord NoStatusAvailable           = 4 ;
const ARM7StatusWord NoStatusVariableAvailable   = 5 ;
const ARM7StatusWord StatusReset                 = 6 ;




/**
 * ARM7 error code values.
 *
 * Error codes in the [0..1023] range are reserved for Ceylan use.
 *
 */

const ARM7ErrorCode ErrorVoluntarilyLeftBlank        =  0 ;
const ARM7ErrorCode UnexpectedSystemCommand          =  1 ;
const ARM7ErrorCode UnexpectedExternalLibraryCommand =  2 ;
const ARM7ErrorCode UnexpectedApplicationCommand     =  3 ;
const ARM7ErrorCode FIFOErrorWhileReading			 =  4 ;
const ARM7ErrorCode FIFOErrorWhileWriting			 =  5 ;
const ARM7ErrorCode NoError 						 =  6 ;
const ARM7ErrorCode NoErrorVariableAvailable		 =  7 ;
const ARM7ErrorCode CommandOverlapping				 =  8 ;
const ARM7ErrorCode UnexpectedBehaviour 			 =  9 ;
const ARM7ErrorCode IPCAlreadyStarted				 = 10 ;
const ARM7ErrorCode IPCAlreadyStopped				 = 11 ;
const ARM7ErrorCode AwokenWithNothingToRead 		 = 12 ;
const ARM7ErrorCode IncorrectInitialStatus			 = 13 ;
const ARM7ErrorCode IncorrectInitialError			 = 14 ;
const ARM7ErrorCode NoErrorAvailable				 = 15 ;
const ARM7ErrorCode UnexpectedCommandCount			 = 16 ;
const ARM7ErrorCode IncorrectApplicationAnswer		 = 17 ;
const ARM7ErrorCode FIFOTimeOutWhileReading 		 = 18 ;
const ARM7ErrorCode FIFOTimeOutWhileWriting 		 = 19 ;

