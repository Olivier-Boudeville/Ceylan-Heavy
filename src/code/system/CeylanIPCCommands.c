#include "CeylanIPCCommands.h"


/**
 * This include file centralizes the system (Ceylan-specific) command
 * identifiers for the FIFO-based IPC, notably for the Nintendo DS.
 *
 * It is to be included by ARM7 C code and ARM9 C++ code.
 *
 * typedef for FIFOCommandID is expected to be already defined (it cannot be
 * defined here as its definition is specific to each ARM: Ceylan::Uint8/uint8).
 *
 * enum not used as are int, thus 32-bit, whereas we want 8 bit.
 *
 */ 


/**
 * System-specific command identifier values range from 0 to 127.
 *
 * @note Ceylan subdivided this range into [0..32], reserved for its own use,
 * and [32..127], reserved for Ceylan-integrating libraries, notably OSDL,
 * which reserved [32..96]. 
 *
 * Hence other libraries can use [97..127], and each application can use
 * [128..255].
 *
 */



/* Section dedicated to commands from the ARM9 to the ARM7. */


const ARM9CommandID HelloToTheARM7       = 0 ;
const ARM9CommandID PingARM7             = 1 ;
const ARM9CommandID StatusInitRequest    = 2 ;
const ARM9CommandID ShutdownIPCRequest   = 3 ;
const ARM9CommandID BatteryStatusRequest = 4 ;
const ARM9CommandID DSTypeRequest        = 5 ;



/* Section dedicated to commands from the ARM7 to the ARM9. */


const ARM7CommandID HelloToTheARM9       = 0 ;
const ARM7CommandID PongARM9             = 1 ;
const ARM7CommandID BatteryStatusAnswer  = 2 ;
const ARM7CommandID DSTypeAnswer         = 3 ;



