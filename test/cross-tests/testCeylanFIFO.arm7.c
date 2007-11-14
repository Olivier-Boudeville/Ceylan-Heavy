

/**
 * ARM7 side of the Ceylan FIFO test.
 *
 * @see testCeylanFIFO.arm9.cc for the peer implementation
 * @see CeylanFIFO.h and CeylanFIFO.cc for the implementation
 * 
 */
 
 
/*
 * Apparently the overhead due to the C++, to the STL and (marginally) to
 * the Ceylan library itself leads to having a ARM7 executable too big to
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



/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 * @see http://www.neimod.com/dstek/dstek2.xml#Interprocessor%20Communication
 *
 */


/* Defines the actual ARM7 status words and error codes */
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

#define CEYLAN_DEBUG_FIFO 1



/* Type definitions */


/* One entry of the FIFO */
typedef uint32 FIFOElement ;

/* Describes a number of FIFO commands. */
typedef uint8 FIFOCommandCount ;




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



/* Normal command counts */


/*
 * Automatically incremented when using the prepareFIFOCommand function, hence
 * not to be especially managed by user code.
 *
 */
FIFOCommandCount localCommandCount = 0 ;


/*
 * Automatically incremented by the handleReceivedCommand function, hence not 
 * to be especially managed by user code.
 *
 */
FIFOCommandCount remoteCommandCount = 0 ;



/* 4-bit only command counts */

/*
 * Automatically incremented by the handleReceivedCommand function, hence not 
 * to be especially managed by user code.
 *
 */
FIFOCommandCount processedCount = 0 ;


/*
 * Automatically incremented by the notifyCommandToARM9 function, hence not 
 * to be especially managed by user code.
 *
 */
FIFOCommandCount sentCount = 0 ;



/* Helper functions */


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
 * Update the status word accordingly.
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
	
	res |= ( id << 24 ) | ( localCommandCount << 16 ) ;
	
	/* Prepare for next command: */
	localCommandCount++ ;
	
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

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO
	
	if ( ! dataAvailableForReading() )
		setError( FIFOErrorWhileReading ) ;
		
	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO


	return res ;
	
}




FIFOElement readBlocking()
{

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO

	
	uint32 attemptCount = 1000 ;
	
	/* Active waiting preferred to atomicSleep(): */

	while ( ! dataAvailableForReading() && attemptCount > 0 )
		 attemptCount-- ;
	
	
	/* readBlocking: never ending ? */
	if ( attemptCount == 0 )
	{
	
		setError( FIFOErrorWhileReading ) ;
				 
		/* Active waiting preferred to atomicSleep(): */
		while( ! dataAvailableForReading() )
			;

		/* Recovered: */
		unsetErrorStatus() ;
		setStatusWord( NoStatusAvailable ) ;
	
	}
	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO
		
		
	FIFOElement res = REG_IPC_FIFO_RX ;
	
	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO


	return res ;
	
}



void write( FIFOElement toSend )
{
	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO


	if ( ! spaceAvailableForWriting() )
		setError( FIFOErrorWhileWriting ) ;
		
		
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
		
		
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}



void writeBlocking( FIFOElement toSend )
{

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO

	/* Active waiting preferred to atomicSleep(): */
	while ( ! spaceAvailableForWriting() )
		;
		
	uint32 attemptCount = 1000 ;
	
	/* Active waiting preferred to atomicSleep(): */

	while ( ! spaceAvailableForWriting() && attemptCount > 0 )
		 attemptCount-- ;
	
	
	/* writeBlocking: never ending ? */
	if ( attemptCount == 0 )
	{
	
		setError( FIFOErrorWhileWriting ) ;
		
		/* Triggers the ARM9 if it can help to make some FIFO room: */
		sendSynchronizeInterruptToARM9() ;
		
		while ( ! spaceAvailableForWriting() )
			;

		/* Recovered: */
		//unsetErrorStatus() ;
		//setStatusWord( NoStatusAvailable ) ;
			
	}
		
		
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO

	
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		setError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}



#include <malloc.h>

void makeCrash()
{

	int a = 1 ;
	
	int b= 0 ;
	
	volatile int c = a / b ;

	c++ ;
	
	int * d = 0 ;
	
	*d = 1 ;
	
	free( d ) ;
	free( d ) ;
	
	//exit(1) ;
}



void sendSumRequest()
{

	// How to prevent this sending being interrupted by an IRQ triggering sends ?
	/* 130: application-specific sum request ID, other bytes unused: */

	
	//setError( 50 ) ;

	writeBlocking( prepareFIFOCommand( 130 ) ) ;
	
	writeBlocking( 234 ) ;
	writeBlocking( 1000 ) ;

	notifyCommandToARM9() ;
	
}



void sendHello()
{

	writeBlocking( prepareFIFOCommand( HelloToTheARM9 ) ) ;

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

	/* Sends computed value: */
	writeBlocking( returnedValue ) ;
	
	notifyCommandToARM9() ;
	
}



void handleSumAnswer()
{

	FIFOElement readElement = readBlocking() ;
	
	if ( readElement != 1234 )
		setError( IncorrectApplicationCommand ) ;
		
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
			
		default:
			setError( UnexpectedApplicationCommand ) ;
			break ;
				
	
	}
		
}



void handleReceivedCommand()
{

	if ( ! dataAvailableForReading() )
		return ;
		
	/* 
	 * Interrupts (if used, FIFO, VBlank, IPCSync) are expected to be
	 * disabled when this function is called.	
	 *
	 */
	 
	static bool CommandInProgress = false ;


	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;
			
		FIFOElement firstElement ;
		FIFOCommandID id ;
		FIFOCommandCount count ;
	
	
		/* At least one first element to read, maybe more: */
		while ( dataAvailableForReading() )
		{

			
			/* Read first the command identifier: */
	
	 		/* readBlocking instead of read: increased safety ? */
			firstElement = readBlocking() ;
		
			count = getFIFOCommandCountFrom( firstElement ) ;
	
			if ( count != remoteCommandCount )
			{
		
				setError( UnexpectedCommandCount ) ;
				/* CommandInProgress still true, hence frozen. */
				return ;	
			
			}
		
			remoteCommandCount++ ;
			
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
				

					if ( statusWordPointer != 0 )
					{
					
						setError( IPCAlreadyStarted ) ;
						readBlocking() ;
						readBlocking() ;
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
						readBlocking() ;
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
										
				} 
				else if ( id == ShutdownIPC )
				{
				
					if ( statusWordPointer == 0 || errorWordPointer == 0 )
					{
					
						setError( IPCAlreadyStopped ) ;
						return ;
						
					}	
					
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
				
					/* unexpected system command id: */
					setError( UnexpectedSystemCommand ) ;

					/*
					LogPlug::error( "unexpected command: "
						+ Ceylan::toNumericalString( id ) + ", ignored." ) ;
					 */							
	
				}
				
				
			} /* id corresponds to a system command */
	
			incrementProcessCount() ;
			
					
		} /* end while */

		CommandInProgress = false ;	
	
	}
	else
	{
	
		setError( CommandOverlapping ) ;
		
	}
	
	/*
	 * The IRQ handler that called this method is responsible for acknowledging
	 * the interrupt that triggered it.
	 *
	 */
					
}



void totot()
{

}



void initCeylanIPC()
{



	/*
	 * IPC_SYNC_IRQ_ENABLE allows the ARM9 to trigger IPC_SYNC IRQ on this
	 * ARM7:
	 *
	 * ('=', not '|=', to nullify the rest of the register, not expecting to
	 * write on ARM9 settings))
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
	irqSet( IRQ_VBLANK, totot ) ;
	
	/* Unleash these IRQ: */
	irqEnable( IRQ_IPC_SYNC | IRQ_VBLANK ) ;


	/*
	 * Wait until the IPC system is up and running (ARM9 handshake performed).
	 *
	 */
	while( statusWordPointer == 0 ) 
		atomicSleep() ;
		
	/*
	 * The problem is that ipcInitialized is never true afterwards...
	 * statusWordPointer always null as well
	 */

	//setStatusWord( ARM7Running ) ;
		
		//sendHello() ;
		//atomicSleep() ;

}






int main(int argc, char ** argv) 
{

//makeCrash() ;		
	
	/* Reset the clock if needed: */
	rtcReset() ;
	irqInit() ;

	SetYtrigger( 80 ) ;
	irqSet( IRQ_VCOUNT, VcountHandler ) ;
	irqEnable( IRQ_VCOUNT ) ;

	IPC->mailBusy = 0 ;

	initCeylanIPC() ;

//#if 0
		
//	setStatusWord( 30 ) ;
	setError( 30 ) ;
	



	uint32 count = 180 ;
		
			
	while( count > 0 )
	{
	
	
		sendSumRequest() ;
		//sendHello() ;
		
		atomicSleep() ;
		//count-- ;
		
	}	
	
	while( false )
	{
	
	
		atomicSleep() ;
		
	}	
//#endif	

	return 0 ;
		
}

