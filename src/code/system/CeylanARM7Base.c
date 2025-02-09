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
 * License and of the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "CeylanARM7Base.h"

/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 *
 * @todo Regularly integrates here newer forms of libnds ARM7 template.
 *
 */


/* Defines the actual Ceylan ARM7 status words and error codes. */
#include "CeylanARM7Codes.h"

/* Defines Ceylan IPC command identifiers. */
#include "CeylanIPCCommands.h"


/*
    PowerManagement bits that libnds doesn't define (yet?).
    Check out "arm7/serial.h".
*/

#define PM_BATTERY_STATUS BIT(0)
#define PM_DSLITE_REG        (4)
#define PM_IS_LITE        BIT(6)


const InterruptMask AllInterruptsDisabled = 0 ;


/*
 * Directly obtained from libnds ARM7 template.
 * This is a stripped-down version (no sound) with FIFO support.
 *
 * @see testCeylanFIFO.arm9.cc and CeylanFIFO.cc
 *
 */

touchPosition first, tempPos ;


/* libnds IPC uses shared variables in the transfer region. */
void VcountHandler() 
{

	/* Updates the button state and the touchscreen: */
	
	static int lastbut = -1 ;
	
	uint16 but=0, x=0, y=0, xpx=0, ypx=0, z1=0, z2=0 ;

	but = REG_KEYXY ;

	if ( ! ( (but ^ lastbut) & (1<<6) ) ) 
	{
 
		tempPos = touchReadXY() ;

		if ( tempPos.x == 0 || tempPos.y == 0 ) 
		{
		
			but |= (1 <<6) ;
			lastbut = but ;
			
		} 
		else 
		{
		
			x   = tempPos.x ;
			y   = tempPos.y ;
			xpx = tempPos.px ;
			ypx = tempPos.py ;
			z1  = tempPos.z1 ;
			z2  = tempPos.z2 ;
			
		}
		
	} 
	else 
	{
		lastbut = but ;
		but |= (1 <<6) ;
	}

	IPC->touchX	  = x ;
	IPC->touchY	  = y ;
	IPC->touchXpx = xpx ;
	IPC->touchYpx = ypx ;
	IPC->touchZ1  = z1 ;
	IPC->touchZ2  = z2 ;
	IPC->buttons  = but ;

}




/* Ceylan FIFO-based IPC section. */


/* CEYLAN_SAFE_FIFO read from CeylanARM7Codes.h, expected set to zero. */



/* Definitions of global variables */

/*
 * 'ARM7StatusWord volatile * statusWordPointer = 0 ;' would not be enough:
 * it would correctly manage the fact that the ARM9 can change the pointed
 * value, but the pointer itself must be volatile too, as it can be change
 * in the ARM7 IRQ handler after a StatusInitRequest command has been received.
 *
 */

/* Pointer to the ARM7 shared status word, allocated by the ARM9. */
ARM7StatusWord volatile * volatile statusWordPointer = 0 ;

/* Pointer to the ARM7 shared error code, allocated by the ARM9. */
ARM7ErrorCode volatile * volatile errorWordPointer = 0 ;


/* Tells whether an active handshake is valid between the two ARMs. */
volatile bool IPCRunning = false ;



/* Normal command counts, can be used in the FIFO command element. */


/*
 * Automatically incremented when using the prepareFIFOCommand function, hence
 * not to be especially managed by user code.
 *
 */
volatile FIFOCommandCount localCommandCount = 0 ;


/*
 * Automatically incremented by the handleReceivedCommand function, hence not 
 * to be especially managed by user code.
 *
 */
volatile FIFOCommandCount remoteCommandCount = 0 ;



/* 4-bit only command counts, to be shared in IPC sync register. */

/*
 * Automatically incremented by the handleReceivedCommand function, hence not 
 * to be especially managed by user code.
 *
 */
volatile FIFOCommandCount processedCount = 0 ;


/*
 * Automatically incremented by the notifyCommandToARM9 function, hence not 
 * to be especially managed by user code.
 *
 */
volatile FIFOCommandCount sentCount = 0 ;




/* Helper functions */


/* Waits a short time slice. Needs the VBlank IRQ to be enabled. */ 
void atomicSleep()
{

	swiWaitForVBlank() ;
	
}






/**
 * Sets the ARM7 status word, for the ARM9.
 *
 * @note If previous status was ARM7InError, will be left as is, so that error
 * status is not lost.
 *
 */
void setStatusWord( ARM7StatusWord newStatus )
{

	if ( statusWordPointer != 0 )
	{
	
		if ( *statusWordPointer != ARM7InError )
			*statusWordPointer = newStatus ;
			
	}
	
}



/**
 * Sets the ARM7 error code, for the ARM9.
 * Updates the status word accordingly.
 *
 * @note If previous error code was not NoError, will be left as is.
 * The first error (most interesting one) is kept.
 *
 */
void setError( ARM7ErrorCode newError )
{
	
	setStatusWord( ARM7InError ) ;
	
	if ( errorWordPointer != 0 )
	{
	
		if ( *errorWordPointer == NoError )
			*errorWordPointer = newError ;
			
	}
	
}



/**
 * Unset any previous error status, for example when the error code has been
 * taken into account already.
 *
 */
void unsetErrorStatus()
{


	if ( statusWordPointer != 0 )
		*statusWordPointer = NoStatusAvailable ;
		
	if ( errorWordPointer != 0 )
		*errorWordPointer = NoError ;
		
}

 

/* Creates a FIFO element appropriate to begin a command for the ARM9. */ 
FIFOElement prepareFIFOCommand( FIFOCommandID id )
{
		
	FIFOElement res = 0 ;

#if CEYLAN_SAFE_FIFO
	
	/* Transmits command count for additional checking between the ARMs: */
	res |= ( id << 24 ) | ( localCommandCount << 16 ) ;

	/* Prepare for next command: */
	localCommandCount++ ;
	
#else // CEYLAN_SAFE_FIFO

	res |= id << 24 ;

#endif // CEYLAN_SAFE_FIFO
	
	return res ;
	
}


/* Retrieves the command identifier from a FIFO command element. */
FIFOCommandID getFIFOCommandIDFrom( FIFOElement element )
{

	return ( element & 0xff000000 ) >> 24 ;
	
}


/* Retrieves the command count (if used) from a FIFO command element. */
FIFOCommandCount getFIFOCommandCountFrom( FIFOElement element )
{

	return ( element & 0x00ff0000 ) >> 16 ;
	
}


/* Gets the count of processed commands for this ARM7, from IPC sync register.*/
FIFOCommandCount getARM7ProcessedCount()
{
	
	/* IPC Remote Status is in bits 0-3: */
	return REG_IPC_SYNC & 0x0f ;
	
}


/* Gets the count of processed commands for the ARM9, from IPC sync register. */
FIFOCommandCount getARM9ProcessedCount()
{
	
	/* IPC Local Status is in bits 8-11: */
	return ( REG_IPC_SYNC & 0x0f00 ) >> 8 ;
	
}



/* Gets the count of processed commands for this ARM7, from local count.*/
FIFOCommandCount getProcessedCount()
{

	/* Will be: 0b0000abcd */
	return ( processedCount & 0x0f ) ;
	
}


/* Gets the count of sent commands for this ARM7, from local count.*/
FIFOCommandCount getSentCount()
{

	return ( sentCount & 0x0f ) ;

}



/*
 * Notifies the ARM9 that it should check its FIFO, should previous
 * notifications on sending be missed, using the IPC sync IRQ.
 *
 */
void sendSynchronizeInterruptToARM9()
{

	/*
	 * Triggers an IRQ on the ARM9 and updates the local processed count:
	 * (not used currently)
	  
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) 
		| IPC_SYNC_IRQ_REQUEST ;
		
	 */	

	REG_IPC_SYNC = REG_IPC_SYNC	| IPC_SYNC_IRQ_REQUEST ;

}


/* Registers the fact that one more command was processed on the ARM7. */
void incrementProcessCount()
{

	processedCount++ ;
	
	/*
	 * Updates the local processed count in IPC register: 
	 *
	 * @note: no IPC_SYNC_IRQ_REQUEST here, ARM9 not notified.
	 *
	 * (not used currently)
	 *
	 
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) ;
	
	 */
		
}



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
InterruptMask setEnabledInterrupts( InterruptMask newMask )
{

	InterruptMask previousMask = REG_IME ;
	
	REG_IME = newMask ;
	
	return previousMask ;

}


/* Warns the ARM9 that a new command has been sent by the ARM7 on the FIFO. */
void notifyCommandToARM9()
{

	sentCount++ ;
	sendSynchronizeInterruptToARM9() ;

}


/* Checks whether there is at least one element waiting in the ARM7 FIFO. */
bool dataAvailableForReading()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) ;
	
}


/* Checks whether there is at least one free slot in the ARM7 FIFO. */
bool spaceAvailableForWriting()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL ) ;

}



/* Reads an element from the ARM7 FIFO, supposing there is at least one. */
FIFOElement read()
{

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_SAFE_FIFO
	
	
	if ( ! dataAvailableForReading() )
		setError( FIFOErrorWhileReading ) ;
		
	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_SAFE_FIFO

	return res ;
	
}



/* Reads an element from the ARM7 FIFO, waiting if needed until there is one. */
FIFOElement readBlocking()
{

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_SAFE_FIFO

	
	uint32 attemptCount = 100000 ;
	
	/* Active waiting preferred to atomicSleep(): */

	while ( ! dataAvailableForReading() && attemptCount > 0 )
		 attemptCount-- ;
		
	/* readBlocking: never ending? */
	if ( attemptCount == 0 )
	{
	
		setError( FIFOTimeOutWhileReading ) ;
				 
		/* Active waiting preferred to atomicSleep(): */
		while( ! dataAvailableForReading() )
			;

		/* Recovered: */
		unsetErrorStatus() ;
		setStatusWord( StatusReset ) ;
	
	}

	
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_SAFE_FIFO
		
		
	FIFOElement res = REG_IPC_FIFO_RX ;
	
	
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_SAFE_FIFO


	return res ;
	
}



/* Writes an element to the ARM7 FIFO, supposing there is room for it. */
void write( FIFOElement toSend )
{
	
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO


	if ( ! spaceAvailableForWriting() )
		setError( FIFOErrorWhileWriting ) ;
		
		
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO
		
		
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO
	
}



/* 
 * Writes an element to the ARM7 FIFO, waiting if needed until there is room
 * for it. 
 */
void writeBlocking( FIFOElement toSend )
{

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO


	uint32 attemptCount = 100000 ;
	
	/* Active waiting preferred to atomicSleep(): */

	while ( ! spaceAvailableForWriting() && attemptCount > 0 )
		 attemptCount-- ;
	
	
	/* writeBlocking: never ending? */
	if ( attemptCount == 0 )
	{
	
		setError( FIFOTimeOutWhileWriting ) ;
		
		/* Triggers the ARM9 if it can help to make some FIFO room: */
		sendSynchronizeInterruptToARM9() ;
		
		while ( ! spaceAvailableForWriting() )
			;

		/* Recovered: */
		unsetErrorStatus() ;
		setStatusWord( StatusReset ) ;
			
	}
		
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO

	
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_SAFE_FIFO
	
}




/* Section dedicated to system-specific (Ceylan) IPC */



/* Manages an IPC request to initialize status words. */	
void handleStatusInitRequest()
{
	
	if ( statusWordPointer != 0 || IPCRunning )
	{
	
		setError( IPCAlreadyStarted ) ;
		return ;
		
	}	
	
	
	/* 
	 * The ARM9 will send the address of the shared ARM7
	 * status word in next element: 
	 */
	statusWordPointer = (volatile ARM7StatusWord*) readBlocking() ;
	
	if ( *statusWordPointer != NoStatusAvailable )
	{
	
		setError( IncorrectInitialStatus ) ;
		return ;
		
	}	
		
	*statusWordPointer = ARM7Running ;
	
	
	errorWordPointer = (volatile ARM7ErrorCode*) readBlocking() ;
	
	if ( *errorWordPointer != NoErrorAvailable )
	{
	
		setError( IncorrectInitialError ) ;
		return ;
		
	}	
		
	*errorWordPointer = NoError ;

	/* Set it last to avoid main loop firing too soon: */
	IPCRunning = true ;
								
} 



/* Manages an IPC request to stop IPC system. */	
void handleShutdownIPCRequest()
{
	
	if ( statusWordPointer == 0 || errorWordPointer == 0 || ! IPCRunning )
	{
	
		setError( IPCAlreadyStopped ) ;
		return ;
		
	}	
	
	IPCRunning = false ;
	
	/* 
	 * Stop the mechanism on the ARM7 side.
	 * 
	 * Ends with ARM7IPCShutdown status (and NoError error 
	 * code) to perform last handshake with ARM9.
	 *
	 * @see FIFO::deactivate
	 *
	 */
	*statusWordPointer = ARM7IPCShutdown ;
	*errorWordPointer = NoError ;

	statusWordPointer = 0 ;
	errorWordPointer = 0 ;
	
	
	REG_IPC_FIFO_CR = REG_IPC_FIFO_CR & ~IPC_FIFO_ENABLE ;
	
	irqDisable( IRQ_IPC_SYNC ) ;
	
	/* 
	 * IRQ_VBLANK not disabled, as can be used for other reasons 
	 *
	 */
	
}



/* Returns the battery status to the ARM9. */
void handleBatteryStatusRequest()
{

	/*
	 * Inspired from Rick Wong (Lick), see http://licklick.wordpress.com/
	 *
	 * Thanks Lick!
	 *
	 */
	 
	/* Prepares answer, last byte of the answer being zero: */
	FIFOElement answer = prepareFIFOCommand( BatteryStatusAnswer ) ;
	
	if ( ! ( readPowerManagement( PM_BATTERY_REG ) & PM_BATTERY_STATUS ) )
		answer |= 1 ;
				
	writeBlocking( answer ) ; 
	
	notifyCommandToARM9() ;

}



/* Returns the type of this DS (fat or lite) to the ARM9. */
void handleDSTypeRequest()
{

	/*
	 * Inspired from Rick Wong (Lick), see http://licklick.wordpress.com/
	 *
	 * Thanks Lick !
	 *
	 */
	 
	/* Prepares answer, last byte of the answer being zero: */
	FIFOElement answer = prepareFIFOCommand( DSTypeAnswer ) ;
	
	/* Interrupts must be disabled for readPowerManagement: */
	if ( readPowerManagement( PM_DSLITE_REG ) & PM_IS_LITE )
		answer |= 2 /* DS Lite */ ;
	else
		answer |= 1 /* DS Fat */ ;
		
	writeBlocking( answer ) ; 
	
	notifyCommandToARM9() ;

}


/* Section dedicated to library integration of Ceylan IPC. */




/* Section dedicated to Ceylan generic IPC dispatching. */


/* Ceylan system-specific command handler. */
void handleReceivedSystemSpecificCommand( FIFOCommandID commandID, 
	FIFOElement firstElement )
{

	/*
	 * Here we are dealing with a system-specific (Ceylan) command.
	 *
	 * @note Cannot use switch here, as if using:
	 * const int MyConstant = 1 ;
	 * switch ( aValue )
	 * {
	 *   case MyConstant:
	 * etc.
	 *
	 * gcc says: 'case label does not reduce to an integer constant'
	 *
	
	switch( commandID )
	{
	
		case HelloToTheARM7:
			// Corresponds to reading zero in the FIFO: 
			setError( UnexpectedBehaviour ) ;
			break ;
			
		case ShutdownIPCRequest:
			handleShutdownIPCRequest() ;
			break ;
			
		case StatusInitRequest:
			handleStatusInitRequest() ;
			break ;
								
		case BatteryStatusRequest:
			handleBatteryStatusRequest() ;
			break ;
			
		case DSTypeRequest:
			handleDSTypeRequest() ;
			break ;
			
		default:
			// Unexpected system command id: 
			setError( UnexpectedSystemCommand ) ;
			break ;		
	
	}

	*/

	if ( commandID == HelloToTheARM7 /* i.e. null FIFO element */ )
		setError( UnexpectedBehaviour ) ;
	else if ( commandID == ShutdownIPCRequest )
		handleShutdownIPCRequest() ;
	else if ( commandID == StatusInitRequest )
		handleStatusInitRequest() ;
	else if ( commandID == BatteryStatusRequest )
		handleBatteryStatusRequest() ;
	else if ( commandID == DSTypeRequest )
		handleDSTypeRequest() ;
	else
		setError( UnexpectedSystemCommand ) ;
		
}




/* 
 * Command dispatcher, between system-specific (Ceylan), integrating libraries
 * (Ceylan) and application-specific ones. 
 *
 * Called by the IPCSYNC handler (main notification used).
 *
 * Called by the VBlank handler, to poll the FIFO queue, as IPC sync IRQ may
 * be lost if sent while managing an IRQ (since then they are disabled),
 * which would delay the processing until next sync IRQ or until the FIFO 
 * queue gets full.
 *
 * @note Relies on external handleReceivedApplicationCommand and 
 * handleReceivedIntegratingLibrarySpecificCommand functions to be defined by
 * the user.
 *
 */
void handleReceivedCommand()
{

	if ( ! dataAvailableForReading() )
	{
	
		/*
		 * Is not a real error, as may be triggered by the ARM9 read/write
		 * blocking operations when waiting for too long the ARM7:
		  
		setError( AwokenWithNothingToRead ) ;	
		 
		 *
		 */
		return ;
	
	}	

		
	/* 
	 * Interrupts (if used, FIFO, VBlank, IPCSync) are expected to be
	 * disabled when this function is called.	
	 *
	 */
	 
#if CEYLAN_SAFE_FIFO

	static bool CommandInProgress = false ;


	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;

#endif // CEYLAN_SAFE_FIFO

			
		FIFOElement firstElement ;
		FIFOCommandID id ;
	
	
		/* At least one first element to read, maybe more: */
		while ( dataAvailableForReading() )
		{
	
			/* Read first the command identifier: */
	
	 		/* readBlocking instead of read: increased safety? */
			firstElement = readBlocking() ;


#if CEYLAN_SAFE_FIFO
		
			FIFOCommandCount count = getFIFOCommandCountFrom( firstElement ) ;
	
			if ( count != remoteCommandCount )
			{
		
				setError( UnexpectedCommandCount ) ;
				
				/* CommandInProgress still true, hence frozen if in safe mode.*/
				return ;	
			
			}
		
			remoteCommandCount++ ;
			
#endif // CEYLAN_SAFE_FIFO


			id = getFIFOCommandIDFrom( firstElement ) ;
			
			if ( id > 127 )
			{
	
				/* It is an application-specific command, relay it: */
				handleReceivedApplicationCommand( id, firstElement ) ;
	
			}
			else 
			{
						
				if ( id < 33 )
				{
			
					/* It is a Ceylan-specific command, relay it: */
					handleReceivedSystemSpecificCommand( id, firstElement ) ;
				
				}
				else	
				{

					/* It is a command for Ceylan, relay it: */
					handleReceivedIntegratingLibrarySpecificCommand( id,
						firstElement ) ;
				
				}
				
			} 
	
			incrementProcessCount() ;
			
					
		} /* end while */


#if CEYLAN_SAFE_FIFO

		CommandInProgress = false ;	
	
	}
	else
	{
	
		setError( CommandOverlapping ) ;
		
	}
	
#endif // CEYLAN_SAFE_FIFO
					
}




/**
 * Sets-up the IPC mechanism by synchronizing with the ARM9 and retrieving
 * the status and error variables used by the ARM7 to report its state.
 *
 */
void initCeylanIPC()
{


	/*
	 * IPC_SYNC_IRQ_ENABLE allows the ARM9 to trigger IPC_SYNC IRQ on this
	 * ARM7:
	 *
	 * ('=', not '|=', to nullify the rest of the register, not expecting to
	 * write on ARM9 settings)
	 *
	 */
	REG_IPC_SYNC = IPC_SYNC_IRQ_ENABLE ;


	/*
	 * First, set-up the FIFO.
	 *
	 * REG_IPC_FIFO_CR is the FIFO *control* register, and:
	 *  - IPC_FIFO_ENABLE enables the FIFO
	 *  - IPC_FIFO_SEND_CLEAR flushes the send FIFO
	 *  - (not used anymore) IPC_FIFO_RECV_IRQ request an IRQ to be triggered 
	 * on a transition, on the receive FIFO, from empty to not empty	 
	 *
	 */
  	REG_IPC_FIFO_CR = IPC_FIFO_ENABLE | IPC_FIFO_SEND_CLEAR ;


	/*
	 * FIFO not empty IRQ (IRQ_FIFO_NOT_EMPTY) not used anymore, as 
	 * IRQ_IPC_SYNC is considered more appropriate.
	 *
	 
	irqSet( IRQ_FIFO_NOT_EMPTY, syncHandlerForFIFO ) ; 
    irqEnable( IRQ_FIFO_NOT_EMPTY | etc.) ;
	
	 */
	 
	 
	/*
	 * Fully managed by the Ceylan FIFO system:
	 *
	 */

	irqSet( IRQ_IPC_SYNC, handleReceivedCommand ) ; 

	/* Needed for atomic sleeps and for increased reliability: */
	/* irqSet( IRQ_VBLANK, 0 ) ; */
	irqSet( IRQ_VBLANK, handleReceivedCommand ) ;
	
	/* Unleashes these IRQ: */
	irqEnable( IRQ_IPC_SYNC | IRQ_VBLANK ) ;


	/*
	 * Wait until the IPC system is up and running (ARM9 handshake performed).
	 *
	 */
	while( ! IPCRunning )
		;


	/* Let some time elapse to get out of the IRQ handler for IPC startup: */
	atomicSleep() ;
	
}



/**
 * Sets-up the sound hardware.
 *
 */
void initCeylanSound()
{

	InterruptMask previous = setEnabledInterrupts( AllInterruptsDisabled ) ;

	powerON( POWER_SOUND ) ;

	writePowerManagement( PM_CONTROL_REG, ( readPowerManagement(PM_CONTROL_REG)
		& ~PM_SOUND_MUTE ) | PM_SOUND_AMP ) ;
	
	SOUND_CR = SOUND_ENABLE | SOUND_VOL( 0x7f ) ;
	IPC->soundData = 0;

	setEnabledInterrupts( previous ) ;

	/* Wait a bit to let sound hardware be initialized: */
	atomicSleep() ;
	
}



/**
 * Sets-up Ceylan and its prerequesites.
 *
 */
void initCeylan()
{

	/* Read user settings from firmware: */
	readUserSettings() ;
	
	/* Reset the clock if needed: */
	rtcReset() ;

	irqInit() ;

	SetYtrigger( 80 ) ;
	irqSet( IRQ_VCOUNT, VcountHandler ) ;
	irqEnable( IRQ_VCOUNT ) ;

	IPC->mailBusy = 0 ;


	initCeylanIPC() ;
	
	initCeylanSound() ;

}

