#ifndef CEYLAN_IPC_COMMANDS_H_
#define CEYLAN_IPC_COMMANDS_H_


/**
 * This include file centralizes the system (Ceylan-specific) command
 * identifiers for the FIFO-based IPC, notably for the Nintendo DS.
 *
 * It is to be included by ARM7 C code and ARM9 C++ code.
 *
 * typedef for FIFOCommandID is expected to be already defined (it cannot be
 * defined here as its definition is specific to each ARM:
 * Ceylan::Uint8/uint8).
 *
 * enum not used as are int thus 32-bit whereas we want 8 bit.
 *
 */ 


/**
 * System-specific command identifier values range from 0 to 127.
 *
 */


/**
 * Identifiers for commands originating from the ARM9 (aimed at the ARM7):
 * 
 */
typedef FIFOCommandID ARM9CommandID ;
 
 
 
/**
 * This is not really a command, as null values can occur before the  
 * FIFO system is up and running on both sides.
 *
 * No parameters set in the command element, no answer expected.
 *
 */
const ARM9CommandID HelloToTheARM7 = 0 ;


/**
 * Sends a ping to the ARM7, which is expected to return a pong command.
 *
 * No parameters set in the only command element, pong answer expected.
 *
 * @see PongARM9
 *
 */
const ARM9CommandID PingARM7 = 1 ;


/**
 * Tells the ARM7 to update its state, and to write it and its error code in
 * the variable whose address is specified in the next element of this command.
 *
 * No parameters set in the command element, next sent element will be the
 * address in question, no answer expected.
 *
 */
const ARM9CommandID SendARM7StatusAndErrorReportAddress = 2 ;


/**
 * Tells the ARM7 to stop its FIFO and its report mechanism.
 *
 * No parameters set in the command element, no next element to send, 
 * no answer expected.
 *
 */
const ARM9CommandID ShutdownIPC = 3 ;




/**
 * Identifiers for commands originating from the ARM7 (aimed at the ARM9):
 * 
 */
typedef FIFOCommandID ARM7CommandID ;


/**
 * This is not really a command, as null values can occur before the  
 * FIFO system is up and running on both sides.
 *
 * No parameters set in the command element, no answer expected.
 *
 */
const ARM7CommandID HelloToTheARM9 = 0 ;


/**
 * Sends a pong to the ARM9, after a ping command has been sent to the ARM7.
 *
 * No parameters set in the only command element, no answer expected.
 *
 * @see PingARM7
 *
 */
const ARM7CommandID PongARM9 = 1 ;




#endif // CEYLAN_IPC_COMMANDS_H_

