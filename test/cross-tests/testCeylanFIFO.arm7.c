

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



/* Defines the ARM7 status word type */
typedef uint16 ARM7StatusWord ;

/* Defines the ARM7 error code type */
typedef uint16 ARM7ErrorCode ;

/* Defines the FIFO command identifier type for the ARM7 */
typedef uint8 FIFOCommandID ;



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
#define CEYLAN_FIFO_USES_VBLANK 1 

/* One entry of the FIFO */
typedef uint32 FIFOElement ;



/* Pointer to the ARM7 shared status word, allocated by the ARM9 */
uint16 * StatusWordPointer = 0 ;
uint16 * ErrorWordPointer = 0 ;



/* Helper functions */


/**
 * Sets the ARM7 status word, for the ARM9.
 *
 * @note If previous status was ARM7InError, will be left as is.
 *
 */
void SetStatusWord( ARM7StatusWord newStatus )
{

	if ( StatusWordPointer != 0 )
	{
	
		if ( *StatusWordPointer != ARM7InError )
			*StatusWordPointer = newStatus ;
			
	}
	
}



/**
 * Sets the ARM7 error code, for the ARM9.
 * Update the status word accordingly.
 *
 * @note If previous error code was not NoError, will be left as is.
 *
 */
void SetError( ARM7ErrorCode newError )
{

	SetStatusWord( ARM7InError ) ;
	
	if ( ErrorWordPointer != 0 )
	{
	
		if ( *ErrorWordPointer == NoError )
			*ErrorWordPointer = newError ;
			
	}
	
}



/**
 * Unset any previous error status, for example when the error code has been
 * taken into account already.
 *
 */
void UnsetErrorStatus()
{


	if ( StatusWordPointer != 0 )
		*StatusWordPointer = NoStatusAvailable ;
		
	if ( ErrorWordPointer != 0 )
		*ErrorWordPointer = NoError ;
		
}

 

FIFOCommandID GetFIFOCommandIDFrom( const FIFOElement * element )
{

	return ( *element & 0xff000000 ) >> 24 ;
	
}



void SetFIFOCommandIDTo( FIFOElement * targetElement, FIFOCommandID id )
{

	(*targetElement) = ( (*targetElement) & 0x00ffffff ) | ( id << 24 ) ;
	
}



void atomicSleep()
{

	swiWaitForVBlank() ;
	
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
		SetError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO
	

	if ( ! dataAvailableForReading() )
		SetError( FIFOErrorWhileReading ) ;
		
	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO


	return res ;
	
}




FIFOElement readBlocking()
{

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO


	/* Active waiting preferred to atomicSleep(): */
	while( ! dataAvailableForReading() )
		;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO
		
		
	FIFOElement res = REG_IPC_FIFO_RX ;
	
	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileReading ) ;

#endif // CEYLAN_DEBUG_FIFO


	return res ;
	
}



void write( FIFOElement toSend )
{
	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO


	if ( ! spaceAvailableForWriting() )
		SetError( FIFOErrorWhileWriting ) ;
		
		
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
		
		
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}



void writeBlocking( FIFOElement toSend )
{

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO


	/* Active waiting preferred to atomicSleep(): */
	while ( ! spaceAvailableForWriting() )
		;
		
		
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO

	
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		SetError( FIFOErrorWhileWriting ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}



/* Example of application-specific command handler */
void handleReceivedApplicationCommand( FIFOCommandID id, FIFOElement element )
{
	
	if ( id == 128 )
	{
			
		/* Command identifier ok, needing the value address now: */
	
		FIFOElement readElement = readBlocking() ;

		/* Add 42 to the value pointed by this address */
		uint32 returnedValue = *((uint32 *) readElement) + 42 ;

		/* sends answer identifier (129): */
	
		FIFOElement commandAnswer ;
		SetFIFOCommandIDTo( &commandAnswer, 129 ) ;
		
		writeBlocking( commandAnswer ) ; 

		/* sends computed value: */
		writeBlocking( returnedValue ) ;
		
	}
	else
	{
	
		/* unexpected id: */
		SetError( UnexpectedApplicationCommand ) ;
		
	}	

	
}



void ManageReceivedFIFOElement()
{
	
	static bool CommandInProgress = false ;

#if CEYLAN_FIFO_USES_VBLANK
	
	/* Disable VBlank and FIFO not empty interrupts: */
	REG_IE &= ~( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;
	
#else // CEYLAN_FIFO_USES_VBLANK

	/* Disable FIFO not empty interrupt: */
	REG_IE &= ~IRQ_FIFO_NOT_EMPTY ;
	
#endif // CEYLAN_FIFO_USES_VBLANK


	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;
			
		FIFOElement firstElement ;
		FIFOCommandID id ;
	
	
		/* At least one first element to read, maybe more: */
		while ( dataAvailableForReading() )
		{

			
			/* Read first the command identifier: */
	
	 		/* readBlocking instead of read: increased safety ? */
			firstElement = readBlocking() ;
		
			id = GetFIFOCommandIDFrom( &firstElement ) ;
			
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
	
					/*
					LogPlug::info( "The ARM9 says hello to the ARM7 !" ) ;
					 */
					 
				} 
				else if ( id == SendARM7StatusAndErrorReportAddress )
				{
				
					/* 
					 * The ARM9 will send the address of the shared ARM7
					 * status word in next element: 
					 */
					StatusWordPointer = (uint16*) readBlocking() ;
					*StatusWordPointer = ARM7Running ;
						
					ErrorWordPointer = StatusWordPointer + 1 ;
					*ErrorWordPointer = NoError ;
						
				} 
				else if ( id == ShutdownIPC )
				{
				
					/* 
					 * Stop the mechanism on the ARM7 side.
					 * 
					 * Ends with ARM7IPCShutdown status (and NoError error 
					 * code) to perform last handshake with ARM9.
					 *
					 * @see FIFO::deactivate
					 *
					 */
					*StatusWordPointer = ARM7IPCShutdown ;
					*ErrorWordPointer = NoError ;

					StatusWordPointer = 0 ;
					ErrorWordPointer = 0 ;
					
					REG_IPC_FIFO_CR &= 
						~( IPC_FIFO_ENABLE | IPC_FIFO_RECV_IRQ ) ;
					
					irqDisable( IPC_FIFO_RECV_IRQ ) ;
					
					/* 
					 * IRQ_VBLANK not disabled as can be used for other reasons 
					 *
					 */
					
				}
				else
				{						
				
					/* unexpected system command id: */
					SetError( UnexpectedSystemCommand ) ;

					/*
					LogPlug::error( "unexpected command: "
						+ Ceylan::toNumericalString( id ) + ", ignored." ) ;
					 */							
	
				}
				
				
			} /* id corresponds to a system command */
	
			CommandInProgress = false ;	
				
		} /* end while */
	
	}

	/*
	 * else ignores the possible IRQ that triggered that method, as this 
	 * event (FIFO not empty) is expected to be managed by the
	 * handleReceivedCommandhandler command.
	 *
	 * The IRQ handler that called this method is responsible for acknowledging
	 * the interrupt that triggered it.
	 *
	 */

	#if CEYLAN_FIFO_USES_VBLANK

	REG_IE = REG_IE | ( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;

	#else // CEYLAN_FIFO_USES_VBLANK

	REG_IE = REG_IE | ( IRQ_FIFO_NOT_EMPTY ) ;

	#endif // CEYLAN_FIFO_USES_VBLANK
			
}



void HandleReceivedFIFOElement()
{


#if CEYLAN_FIFO_USES_VBLANK
	
	/* Disable VBlank and FIFO not empty interrupts: */
	REG_IE &= ~( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;
	
#else // CEYLAN_FIFO_USES_VBLANK

	/* Disable FIFO not empty interrupt: */
	REG_IE &= ~IRQ_FIFO_NOT_EMPTY ;

#endif // CEYLAN_FIFO_USES_VBLANK
	
	
	/* Note that the next test must be useless: */

	/* Function shared with the VBlank handler: */
	if ( dataAvailableForReading() )
		ManageReceivedFIFOElement() ;
	
	/* Notify this interrupt has been managed: */
	REG_IF |= IRQ_FIFO_NOT_EMPTY ;			


#if CEYLAN_FIFO_USES_VBLANK

	/* Reactivate VBlank and FIFO not empty interrupts: */
	REG_IE |= ( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;

#else // CEYLAN_FIFO_USES_VBLANK

	/* Reactivate FIFO not empty interrupt: */
	REG_IE |= IRQ_FIFO_NOT_EMPTY ;

#endif // CEYLAN_FIFO_USES_VBLANK
				
	
}



void HandleVBlank() 
{

	/* Disable VBlank and FIFO not empty interrupts: */
	REG_IE &= ~( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;


	/* Polling-based parachute, should a FIFO IRQ trigger have been lost: */
	if ( dataAvailableForReading() )
		ManageReceivedFIFOElement() ;


	/* Notify this interrupt has been managed: */
	REG_IF |= IRQ_VBLANK ;			

	/* Reactivate VBlank and FIFO not empty interrupts: */
	REG_IE |= ( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;
	
}



void initFIFO()
{

	irqSet( IRQ_FIFO_NOT_EMPTY, HandleReceivedFIFOElement ) ; 
	irqEnable( IRQ_FIFO_NOT_EMPTY ) ;

#if CEYLAN_FIFO_USES_VBLANK

	irqSet( IRQ_VBLANK, HandleVBlank ) ;
	irqEnable( IRQ_VBLANK ) ;
	
#endif // CEYLAN_FIFO_USES_VBLANK
	 
	/*
	 * REG_IPC_FIFO_CR is the FIFO *control* register, and:
	 *  - IPC_FIFO_ENABLE enables the FIFO
	 *  - IPC_FIFO_SEND_CLEAR flushes the send FIFO
	 *  - IPC_FIFO_RECV_IRQ request an IRQ to be triggered on a transition, on
	 * the receive FIFO, from empty to not empty	 
	 *
	 */
	REG_IPC_FIFO_CR = 
		IPC_FIFO_ENABLE | IPC_FIFO_SEND_CLEAR | IPC_FIFO_RECV_IRQ ;

}



int main(int argc, char ** argv) 
{
	
	/* Reset the clock if needed : */
	rtcReset() ;
		
	irqInit() ;

	initFIFO() ;
	 
	SetYtrigger( 80 ) ;
	irqSet( IRQ_VCOUNT, VcountHandler ) ;
	irqEnable( IRQ_VCOUNT ) ;

	IPC->mailBusy = 0 ;

	while( true )
		atomicSleep() ;
		
}


