/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#ifndef CEYLAN_ARM7_BASE_H_
#define CEYLAN_ARM7_BASE_H_


#include "CeylanARM7Codes.h" /* for ARM7StatusWord and al */


/*
 * Ceylan ARM7 code base centralizing all main code for that processor, notably
 * for IPC management.
 *
 * Later: will offer Wifi facilities.
 *
 * Sound, including sounds (PCM and IMA ADPCM) and MP3 music is not managed
 * here, as it depends on the *.osdl.* file formats. 
 * @see Helix-OSDL for that.
 *
 * User code is expected to define only two callback functions:
 * 'void handleReceivedApplicationCommand( FIFOCommandID commandID, 
 *     FIFOElement element )', which can be void or better can only contain:
 * 'setError( UnexpectedApplicationCommand ) ;'
 *
 *  - and -
 *
 * 'void handleReceivedIntegratingLibrarySpecificCommand( FIFOCommandID
 * commandID, FIFOElement firstElement )', which can be void or better can 
 * only contain:
 * 'setError( UnexpectedExternalLibraryCommand ) ;'
 *
 * ..plus, of course, the main() function, which can be inspired from the
 * examples.
 *
 * @note This Ceylan base remains lightweight, as otherwise library integrating
 * Ceylan (ex: OSDL) would have to redefine mechanisms while still linking with
 * the Ceylan counterpart version.
 *
 * @see testCeylanSound.arm7.c
 *
 */


/*
 * Apparently the overhead due to the C++, to the STL and (marginally) to
 * the Ceylan library itself leads to having an ARM7 executable too big to
 * fit in its IWRAM.
 *
 * Hence for the moment the inclusion of the Ceylan header is commented out
 * and libnds is directly used instead.
 *
 */



/* Type definitions */


/** One of the 16 entries of the FIFO. */
typedef uint32 FIFOElement ;


/** Describes a number of FIFO commands. */
typedef uint8 FIFOCommandCount ;


/** For buffers. */
typedef char Byte ;


/** For buffers. */
typedef uint32 BufferSize ;



/** Masks describing which interrupts are enabled. */
typedef int InterruptMask ;


/** To specify that all interrupts are to disabled (null value). */
extern const InterruptMask AllInterruptsDisabled ;


/** Waits a short time slice. Needs the VBlank IRQ to be enabled. */ 
void atomicSleep() ;



/**
 * Sets the ARM7 status word, for the ARM9.
 *
 * @note If previous status was ARM7InError, will be left as is, so that error
 * status is not lost.
 *
 */
void setStatusWord( ARM7StatusWord newStatus ) ;



/**
 * Sets the ARM7 error code, for the ARM9.
 * Updates the status word accordingly.
 *
 * @note If previous error code was not NoError, will be left as is.
 * The first error (most interesting one) is kept.
 *
 */
void setError( ARM7ErrorCode newError ) ;



/**
 * Unset any previous error status, for example when the error code has been
 * taken into account already.
 *
 */
void unsetErrorStatus() ;



/** Creates a FIFO element appropriate to begin a command for the ARM9. */ 
FIFOElement prepareFIFOCommand( FIFOCommandID id ) ;



/**
 * Sets the current set of interrupts enabled.
 *
 * @param newMask the masks describing all the interrupts that are
 * to be enabled.
 *
 * @return The previous mask that was used, before being replaced by
 * the specified one.
 *
 * @note Usually used to deactivate all IRQ.
 *
 */
InterruptMask setEnabledInterrupts( InterruptMask newMask ) ;



/** Warns the ARM9 that a new command has been sent by the ARM7 on the FIFO. */
void notifyCommandToARM9() ;




/* I/O section. */



/** Reads an element from the ARM7 FIFO, supposing there is at least one. */
FIFOElement read() ;


/** Reads an element from the ARM7 FIFO, waiting if needed until there is one.*/
FIFOElement readBlocking() ;


/** Writes an element to the ARM7 FIFO, supposing there is room for it. */
void write( FIFOElement toSend ) ;


/**
 * Writes an element to the ARM7 FIFO, waiting if needed until there is room
 * for it. 
 */
void writeBlocking( FIFOElement toSend ) ;


/**
 * Manages a Ceylan-specific command.
 *
 * @note defined by this Ceylan ARM7 base.
 *
 */
void handleReceivedSystemSpecificCommand( FIFOCommandID commandID, 
	FIFOElement firstElement ) ;


/**
 * Integrating library-specific (ex: OSDL) command handler, declared here but
 * to be defined by user code, typically the OSDL library, if used.
 *
 * @see testCeylanSound.arm7.c for an example
 *
 * @note Must be defined by the user.
 *
 */
void handleReceivedIntegratingLibrarySpecificCommand( FIFOCommandID commandID,
	FIFOElement firstElement ) ;


/**
 * Application-specific command handler, declared here but to be defined by
 * user code.
 *
 * @see testCeylanSound.arm7.c for an example
 *
 * @note Must be defined by the user.
 *
 */
void handleReceivedApplicationCommand( FIFOCommandID commandID, 
	FIFOElement element ) ;


/* Initializes Ceylan and its prerequisites. */
void initCeylan() ;



#endif // CEYLAN_ARM7_BASE_H_

