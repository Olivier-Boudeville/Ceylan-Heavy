

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
 * Directly obtained from libnds ARM7 template.
 * This is a stripped-down version (no sound) with FIFO support.
 *
 * @see testCeylanFIFO.arm9.cc and CeylanFIFO.cc
 *
 */


touchPosition first, tempPos ;


void VcountHandler() 
{

	// Updates the button state and the touchscreen:
	
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



typedef uint32 FIFOElement ;
typedef uint8 FIFOCommandID ;


/* Pointer to the ARM7 shared status word, specified by the ARM9 */
uint32 * StatusWordPointer = 0 ;


/* Helper functions */


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
	
	return REG_IPC_FIFO_RX ;
	
}



FIFOElement FIFO::readBlocking() throw()
{

	while( ! dataAvailableForReading() )
		atomicSleep() ;
		
	return REG_IPC_FIFO_RX ;
	
}



void FIFO::write( FIFOElement toSend ) throw( FIFOFull )
{
	
	REG_IPC_FIFO_TX = toSend ;
	
}



void FIFO::writeBlocking( FIFOElement toSend ) throw()
{

	while ( ! spaceAvailableForWriting() )
		atomicSleep() ;
	
	REG_IPC_FIFO_TX = toSend ;
	
}



/* Example of application-specific command handler */
void handleReceivedApplicationCommand( FIFOCommandID id, FIFOElement element )
{

	
	if ( id == 128 )
	{
			
		/* Command identifier ok, needing the value address now: */
	
		/* FIFOElement readElement = readBlocking() ; */
	
		/* Active waiting */
		while ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY )
			;
	
		FIFOElement readElement = REG_IPC_FIFO_RX ;

		uint32 returnedValue = *((uint32 *) readElement) + 42 ;

		/* sends answer identifier (129): */
	
		FIFOElement commandAnswer ;
		SetFIFOCommandIDTo( &commandAnswer, 129 ) ;
		
		/* writeBlocking( commandAnswer ) ; */

		/* Active waiting */
		while ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL )
			;
	
		REG_IPC_FIFO_TX = commandAnswer ;
	
		/* sends computed value: */
	
		/* writeBlocking( returnedValue ) ; */

		/* Active waiting */
		while ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL )
			;
	
		REG_IPC_FIFO_TX = returnedValue ;

	}
	else
	{
	
		/* unexpected id, sending error command: */
		
		/* Active waiting */
		while ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL )
			;
		
		/* Application-specific error on the ARM7 side : */
		REG_IPC_FIFO_TX = 0x02000000 ;
		
	}	
	
}



void HandleReceivedFIFOElement()
{
	
	static bool CommandInProgress = false ;


	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;
	
		FIFOElement firstElement ;
		FIFOCommandID id ;
	
	
		/* while ( dataAvailableForReading() ) */
		while ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
		{

			/* Read first the command identifier: */
	
			/* firstElement = read() ; */
			firstElement = REG_IPC_FIFO_RX ;
		
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
				 */
				switch( id )
				{
	
					case 0:
						/*
						LogPlug::info( "The ARM9 says hello to the ARM7 !" ) ;
						 */
						break ;
						
					case 3:	
						/* 
						 * The ARM9 sent the address of the shared ARM7 status 
						 * word: 
						 */
						StatusWordPointer
					default:
						/* Sending error command: */
						/* Active waiting */
						while ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL )
							;
						/* System-specific (Ceylan) error on the ARM7 side: */
						REG_IPC_FIFO_TX = 0x01000000 ;
						
						/*
						LogPlug::error( "FIFO::handleReceivedFIFOElement: "
							"unexpected command: "
							+ Ceylan::toNumericalString( id ) + ", ignored." ) ;
						 */							
						break ;
	
				}
				
			}
	
			CommandInProgress = false ;	
	
	
		} /* end while */
	
	}
	else /* CommandInProgress is true */
	{
	
		/* Ignores the IRQ, as expected to be managed by the handler: */
		return ;
		
	}
	
	
		
}



void HandleVBlank() 
{

	/* Polling-based parachute, should a FIFO IRQ trigger have been lost: */
	if ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
		HandleReceivedFIFOElement() ;
		
}



int main(int argc, char ** argv) 
{
	
	/* Reset the clock if needed : */
	rtcReset() ;

	/* FIFO constructor: */
		
	irqInit() ;

	irqSet( IRQ_FIFO_NOT_EMPTY, HandleReceivedFIFOElement ) ; 
	irqSet( IRQ_VBLANK, HandleVBlank ) ;

	irqEnable( IRQ_FIFO_NOT_EMPTY ) ;

	
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



	SetYtrigger( 80 ) ;
	irqSet( IRQ_VCOUNT, VcountHandler ) ;
	irqEnable( IRQ_VBLANK | IRQ_VCOUNT ) ;

	IPC->mailBusy = 0 ;

	while ( true )
		atomicSleep() ;
		
}


