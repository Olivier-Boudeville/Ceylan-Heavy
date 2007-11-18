

/**
 * ARM7 side of the Ceylan FIFO test.
 *
 * @see testCeylanFIFO.arm9.cc for the peer implementation
 * @see CeylanFIFO.h and CeylanFIFO.cc for its base implementation
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
 
#define USE_CEYLAN 0 

#if USE_CEYLAN

#include "Ceylan.h"

#else // USE_CEYLAN

#define ARM7
#include "nds.h"

#endif // USE_CEYLAN


#include "stdlib.h" // for rand



/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 * @see http://www.neimod.com/dstek/dstek2.xml#Interprocessor%20Communication
 *
 */


/* Defines the actual ARM7 status words and error codes, and CEYLAN_SAFE_FIFO */
#include "CeylanARM7Codes.h"

/* Defines IPC command identifiers */
#include "CeylanIPCCommands.h"



/*
 * Directly obtained from libnds ARM7 template.
 * This is a stripped-down version (no sound) with FIFO support.
 *
 * @see testCeylanFIFO.arm9.cc and CeylanFIFO.cc
 *
 */


touchPosition first, tempPos ;


/* libnds IPC uses shared variables in the transfer region */
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




/* Ceylan FIFO-based IPC section */



/* CEYLAN_SAFE_FIFO read from CeylanARM7Codes.h */

/* Disturbs tests with random waitings */
#define CEYLAN_TEST_WITH_RANDOM 1

/* No CEYLAN_DEBUG_FIFO here */



/* Type definitions */


/* One entry of the FIFO */
typedef uint32 FIFOElement ;

/* Describes a number of FIFO commands. */
typedef uint8 FIFOCommandCount ;


/* For buffers */
typedef char Byte ;

/* For buffers */
typedef uint16 Size ;


/* Masks describing which interrupts are enabled. */
typedef int InterruptMask ;


/* To specify that all interrupts are to disabled. */
const InterruptMask AllInterruptsDisabled = 0 ;



/* Definitions of global variables */

/*
 * 'ARM7StatusWord volatile * statusWordPointer = 0 ;' would not be enough:
 * it would correctly manage the fact that the ARM9 can change the pointed
 * value, but the pointer itself must be volatile too, as it can be change
 * in the IRQ handler after a SendARM7StatusAndErrorReportAddress command
 * has been received.
 *
 */

/* Pointer to the ARM7 shared status word, allocated by the ARM9 */
ARM7StatusWord volatile * volatile statusWordPointer = 0 ;

/* Pointer to the ARM7 shared error code, allocated by the ARM9 */
ARM7ErrorCode volatile * volatile errorWordPointer = 0 ;


volatile bool IPCRunning = false ;



/* Normal command counts */


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



/* 4-bit only command counts */

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
 * @note If previous status was ARM7InError, will be left as is.
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

 
 
FIFOElement prepareFIFOCommand( FIFOCommandID id )
{
		
	FIFOElement res = 0 ;

#if CEYLAN_SAFE_FIFO
	
	res |= ( id << 24 ) | ( localCommandCount << 16 ) ;

	/* Prepare for next command: */
	localCommandCount++ ;
	
#else // CEYLAN_SAFE_FIFO

	res |= id << 24 ;

#endif // CEYLAN_SAFE_FIFO
	
	return res ;
	
}



FIFOCommandID getFIFOCommandIDFrom( FIFOElement element )
{

	return ( element & 0xff000000 ) >> 24 ;
	
}



FIFOCommandCount getFIFOCommandCountFrom( FIFOElement element )
{

	return ( element & 0x00ff0000 ) >> 16 ;
	
}



FIFOCommandCount getARM7ProcessedCount()
{
	
	/* IPC Remote Status is in bits 0-3: */
	return REG_IPC_SYNC & 0x0f ;
	
}



FIFOCommandCount getARM9ProcessedCount()
{
	
	/* IPC Local Status is in bits 8-11: */
	return ( REG_IPC_SYNC & 0x0f00 ) >> 8 ;
	
}



FIFOCommandCount getProcessedCount()
{

	/* Will be: 0000abcd */
	return ( processedCount & 0x0f ) ;
	
}



FIFOCommandCount getSentCount()
{

	return ( sentCount & 0x0f ) ;

}



void sendSynchronizeInterruptToARM9()
{

	/* Triggers on IRQ on the ARM9 and specifies the local processed count: */
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) 
		| IPC_SYNC_IRQ_REQUEST ;

}



void incrementProcessCount()
{

	processedCount++ ;
	
	/*
	 * Updates the local processed count in IPC register: 
	 *
	 * @note: no IPC_SYNC_IRQ_REQUEST here, ARM9 not notified.
	 *
	 */
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) ;
		
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
 */
InterruptMask setEnabledInterrupts( InterruptMask newMask )
{

	InterruptMask previousMask = REG_IME ;
	
	REG_IME = newMask ;
	
	return previousMask ;

}



void notifyCommandToARM9()
{

	sentCount++ ;
	sendSynchronizeInterruptToARM9() ;

}



bool dataAvailableForReading()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) ;
	
}



bool spaceAvailableForWriting()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL ) ;

}




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
		
	/* readBlocking: never ending ? */
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
	
	
	/* writeBlocking: never ending ? */
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




/*
 * Disturbs the test by sometimes adding random delays.
 *
 */
void disturbTest()
{

#if CEYLAN_TEST_WITH_RANDOM

	static unsigned int count = 0 ;
	
	if ( count++ % 7 == 0 )
		swiDelay( /* cycles */ rand() % 500 + 1 ) ;

#endif // CEYLAN_TEST_WITH_RANDOM

}



void sendSumRequest()
{

	InterruptMask previous = setEnabledInterrupts( AllInterruptsDisabled ) ;

	/* 130: application-specific sum request ID, other bytes unused: */
	writeBlocking( prepareFIFOCommand( 130 ) ) ;
	
	writeBlocking( 234 ) ;
	writeBlocking( 1000 ) ;
	
	disturbTest() ;
	
	setEnabledInterrupts( previous ) ;
	
	notifyCommandToARM9() ;

}



void sendHello()
{

	InterruptMask previous = setEnabledInterrupts( AllInterruptsDisabled ) ;

	writeBlocking( prepareFIFOCommand( HelloToTheARM9 ) ) ;

	setEnabledInterrupts( previous ) ;
	
	notifyCommandToARM9() ;

}



void handleComputeRequest()
{
					
	/* Command identifier ok, needing the value now: */
	
	FIFOElement readElement = readBlocking() ;

	/* Add 42 to the specified value: */
	uint32 returnedValue = readElement + 42 ;

	/* Sends answer identifier (129), other bytes unused: */		
	writeBlocking( prepareFIFOCommand( 129 ) ) ; 

	disturbTest() ;
	
	/* Sends computed value: */
	writeBlocking( returnedValue ) ;
	
	notifyCommandToARM9() ;
	
}



void handleSumAnswer()
{

	FIFOElement readElement = readBlocking() ;
	
	disturbTest() ;
	
	if ( readElement != 1234 )
		setError( IncorrectApplicationAnswer ) ;
		
}
  
  

void handleBufferSharing()  
{

	/* Reads the buffer address: */
	Byte * buf = (Byte *) readBlocking() ; 

	/* Reads the buffer size: */
	Size size = (Size) readBlocking() ; 
	
	/* Scan it for expected value: */
	
	Size incorrectCount = 0 ;
	
	Size indexOfFirstIncorrect = 0 ;
	
	const Byte Filler = 170 ;
	
	Size i ;
	
	for ( i = 0; i < size; i++ )
	{
	
		if ( buf[i] != Filler )
		{
		
			incorrectCount++ ; 	
		
			if ( indexOfFirstIncorrect == 0 )
				indexOfFirstIncorrect = i ;
				
		}	
	
	}

	/* Sends answer identifier (136): */		
	
	FIFOElement firstElement = prepareFIFOCommand( 136 ) ;
	
	/* Store count in command element: */
	firstElement = ( firstElement & 0xffff0000 ) 
		| ( (FIFOElement) incorrectCount ) ;
	
	writeBlocking( firstElement ) ; 

	writeBlocking( (FIFOElement) indexOfFirstIncorrect ) ;
	
	disturbTest() ;
		
	notifyCommandToARM9() ;
		
}



/* Example of application-specific command handler */
void handleReceivedApplicationCommand( FIFOCommandID id, FIFOElement element )
{
		
	switch ( id )
	{
	
		case 128:
			handleComputeRequest() ;
			break ;
		
		case 131:
			handleSumAnswer() ;
			break ;
			
		case 135:
			handleBufferSharing() ;
			break ;
			
		default:
			setError( UnexpectedApplicationCommand ) ;
			break ;
				
	}
		
}



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
	
	 		/* readBlocking instead of read: increased safety ? */
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
					
				/*
				 * Here we are dealing with a system-specific (Ceylan) 
				 * command.
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
				 */
				if ( id == HelloToTheARM7 )
				{
	
					setError( UnexpectedBehaviour ) ;
					
					/*
					LogPlug::info( "The ARM9 says hello to the ARM7 !" ) ;
					 */
					 
				} 
				else if ( id == SendARM7StatusAndErrorReportAddress )
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
					statusWordPointer = (volatile ARM7StatusWord*)
						readBlocking() ;
					
					if ( *statusWordPointer != NoStatusAvailable )
					{
					
						setError( IncorrectInitialStatus ) ;
						return ;
						
					}	
						
					*statusWordPointer = ARM7Running ;
			
				
					errorWordPointer = (volatile ARM7ErrorCode*) 
						readBlocking() ;
					
					if ( *errorWordPointer != NoErrorAvailable )
					{
					
						setError( IncorrectInitialError ) ;
						return ;
						
					}	
						
					*errorWordPointer = NoError ;

					/* Set it last to avoid main loop firing too soon: */
					IPCRunning = true ;
					
										
				} 
				else if ( id == ShutdownIPC )
				{
				
					if ( statusWordPointer == 0 || errorWordPointer == 0
						|| ! IPCRunning )
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
					 * IRQ_VBLANK not disabled as can be used for other reasons 
					 *
					 */
					
					return ;
					
				}
				else
				{						
				
					/* Unexpected system command id: */
					setError( UnexpectedSystemCommand ) ;

					/*
					LogPlug::error( "unexpected command: "
						+ Ceylan::toNumericalString( id ) + ", ignored." ) ;
					 */							
	
				}
				
				
			} /* id corresponds to a system command */
	
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



/*
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

	/* Needed for atomic sleeps: */
	irqSet( IRQ_VBLANK, 0 ) ;
	
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




int main(int argc, char ** argv) 
{

	/* Reset the clock if needed: */
	rtcReset() ;

	irqInit() ;

	SetYtrigger( 80 ) ;
	irqSet( IRQ_VCOUNT, VcountHandler ) ;
	irqEnable( IRQ_VCOUNT ) ;

	IPC->mailBusy = 0 ;

	initCeylanIPC() ;


	/* Uncomment to test other features quietly: uint32 count = 0 ; */
	uint32 count = 2000000 ;
		
	while( count > 0 && IPCRunning )
	{
	
		sendSumRequest() ;

		/*
		 * Without a bit of waiting, apparently the ARM7 succeeds in flooding
		 * the ARM9: only sum requests are managed, no compute request is
		 * managed.
		 *
		 */
		if ( count % 50 == 0 )
			atomicSleep() ;

		count-- ;
		
	}	
	
	
	/*
	 * Wait for ever, otherwise the runtime will believe the ROM has crashed.
	 *
	 */
	while( true )
		atomicSleep() ;

}

