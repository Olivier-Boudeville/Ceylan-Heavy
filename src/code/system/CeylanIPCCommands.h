/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_IPC_COMMANDS_H_
#define CEYLAN_IPC_COMMANDS_H_


#include "CeylanARM7Codes.h" // for FIFOCommandID


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




/**
 * Identifiers for commands originating from the ARM9 (aimed at the ARM7):
 * 
 */
typedef FIFOCommandID ARM9CommandID ;
 
 
 
 
// Section dedicated to commands from the ARM9 to the ARM7.
 
 
/**
 * This is not really a command, as null values can occur before the  
 * FIFO system is up and running on both sides.
 *
 * No parameters set in the command element, no answer expected.
 *
 */
extern CEYLAN_DLL const ARM9CommandID HelloToTheARM7 ;



/**
 * Sends a ping to the ARM7, which is expected to return a pong command.
 *
 * No parameters set in the only command element, pong answer expected.
 *
 * @see PongARM9
 *
 */
extern CEYLAN_DLL const ARM9CommandID PingARM7 ;



/**
 * Tells the ARM7 to update its state, and to write it and its error code in
 * the variable whose address is specified in the next element of this command.
 *
 * No parameters set in the command element, next sent element will be the
 * address in question, no answer expected.
 *
 */
extern CEYLAN_DLL const ARM9CommandID StatusInitRequest ;



/**
 * Tells the ARM7 to stop its FIFO and its report mechanism.
 *
 * No parameters set in the command element, no next element to send, 
 * no answer expected.
 *
 */
extern CEYLAN_DLL const ARM9CommandID ShutdownIPCRequest ;



/**
 * Tells the ARM7 to send back the current battery status.
 *
 * No parameters set in the command element, no next element to send, but
 * an answer is expected.
 *
 * @see BatteryStatusAnswer
 *
 */
extern CEYLAN_DLL const ARM9CommandID BatteryStatusRequest ;



/**
 * Tells the ARM7 to send back the actual DS type.
 *
 * No parameters set in the command element, no next element to send, but
 * an answer is expected.
 *
 * @see DSTypeAnswer
 *
 */
extern CEYLAN_DLL const ARM9CommandID DSTypeRequest ;




////////////////////////////////////////////////////////////////////////////////



// Section dedicated to commands from the ARM7 to the ARM9.


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
extern CEYLAN_DLL const ARM7CommandID HelloToTheARM9 ;


/**
 * Sends a pong to the ARM9, after a ping command has been sent to the ARM7.
 *
 * No parameters set in the only command element, no answer expected.
 *
 * @see PingARM7
 *
 */
extern CEYLAN_DLL const ARM7CommandID PongARM9 ;


/**
 * Used by the ARM7 to return the current battery status.
 *
 * @see BatteryStatusRequest
 *
 */
extern CEYLAN_DLL const ARM7CommandID BatteryStatusAnswer ;


/**
 * Used by the ARM7 to return the DS type.
 *
 * @see DSTypeRequest
 *
 */
extern CEYLAN_DLL const ARM7CommandID DSTypeAnswer ;



#endif // CEYLAN_IPC_COMMANDS_H_

